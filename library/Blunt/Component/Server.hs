{-# LANGUAGE TypeFamilies #-}

module Blunt.Component.Server where

import qualified Blunt.Component.Common as Common
import qualified Blunt.Component.Environment as Environment
import qualified Blunt.Component.Logs as Logs
import qualified Blunt.Component.Metrics as Metrics
import qualified Blunt.Version as Version
import Control.Category ((>>>))
import qualified Control.Concurrent as Concurrent
import qualified Control.Exception as Exception
import qualified Control.Newtype as Newtype
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.ByteString.Lazy as ByteStringLazy
import Data.Function ((&))
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.Gzip as Gzip
import qualified Ratel.Wai as Ratel

data Server = Server
    { serverThreadId :: Concurrent.ThreadId
    }

instance Common.Component Server where
    type Dependencies Server = (Environment.Environment, Logs.Logs, Metrics.Metrics, (Logs.Logs, Metrics.Metrics) -> Wai.Application)
    start (environment,logs,metrics,makeApplication) = do
        let application =
                (makeMiddleware environment logs metrics)
                    (makeApplication (logs, metrics))
        let host =
                environment & Environment.environmentServerHost &
                Newtype.unpack
        let port = Environment.environmentServerPort environment
        let serverName = "blunt-" ++ Version.versionString & ByteString.pack
        let settings =
                Warp.defaultSettings & Warp.setHost host & Warp.setPort port &
                Warp.setServerName serverName
        threadId <-
            Concurrent.forkFinally
                (Warp.runSettings settings application)
                (\result ->
                      case result of
                          Left exception -> Exception.throwIO exception
                          Right value -> return value)
        return
            Server
            { serverThreadId = threadId
            }
    stop server = do
        server & serverThreadId & Concurrent.killThread

makeMiddleware :: Environment.Environment
               -> Logs.Logs
               -> Metrics.Metrics
               -> Wai.Middleware
makeMiddleware environment logs metrics =
    honeybadgerMiddleware environment >>>
    exceptionMiddleware logs metrics >>>
    logsMiddleware logs >>> metricsMiddleware metrics >>> Gzip.gzip Gzip.def

honeybadgerMiddleware :: Environment.Environment -> Wai.Middleware
honeybadgerMiddleware environment =
    let apiKey = Environment.environmentHoneybadgerApiKey environment
    in if null apiKey
           then id
           else Ratel.ratelMiddleware apiKey Nothing

logsMiddleware :: Logs.Logs -> Wai.Middleware
logsMiddleware logs handle request respond = do
    let logRequest = do
            let method = ByteString.unpack (Wai.requestMethod request)
            let path = ByteString.unpack (Wai.rawPathInfo request)
            let query = ByteString.unpack (Wai.rawQueryString request)
            let host = show (Wai.remoteHost request)
            let message =
                    "Starting " ++
                    method ++ " " ++ path ++ query ++ " for " ++ host
            Logs.logsNotice logs message
            Logs.logsDebug logs (show request)
    let logResponse response = do
            let method = ByteString.unpack (Wai.requestMethod request)
            let path = ByteString.unpack (Wai.rawPathInfo request)
            let query = ByteString.unpack (Wai.rawQueryString request)
            let host = show (Wai.remoteHost request)
            let status = Wai.responseStatus response
            let code = HTTP.statusCode status
            let phrase = ByteString.unpack (HTTP.statusMessage status)
            let message =
                    "Finished " ++
                    method ++
                    " " ++
                    path ++
                    query ++
                    " for " ++ host ++ " with " ++ show code ++ " " ++ phrase
            if code >= 500
                then Logs.logsError logs message
                else if code >= 400
                         then Logs.logsWarning logs message
                         else Logs.logsNotice logs message
    logRequest
    handle
        request
        (\response ->
              do logResponse response
                 respond response)

metricsMiddleware :: Metrics.Metrics -> Wai.Middleware
metricsMiddleware metrics handle request respond = do
    Metrics.metricsCounter metrics "server.request" 1
    Metrics.metricsTimed
        metrics
        "server.response_duration_s"
        (do handle
                request
                (\response ->
                      do let status = Wai.responseStatus response
                         let code = HTTP.statusCode status
                         Metrics.metricsCounter
                             metrics
                             ("server.response_" ++ show code)
                             1
                         respond response))

exceptionMiddleware :: Logs.Logs -> Metrics.Metrics -> Wai.Middleware
exceptionMiddleware logs metrics handle request respond = do
    Exception.catch
        (handle
             request
             (\response ->
                   respond response))
        (\exception ->
              do let _ = exception :: Exception.SomeException
                 Logs.logsError logs (show exception)
                 Metrics.metricsCounter metrics "server.exception" 1
                 let response =
                         Wai.responseLBS HTTP.status500 [] ByteStringLazy.empty
                 respond response)
