{-# LANGUAGE TypeFamilies #-}

module Blunt.Component.Server where

import qualified Blunt.Component.Common as Common
import qualified Blunt.Component.Environment as Environment
import qualified Blunt.Component.Logs as Logs
import qualified Blunt.Component.Metrics as Metrics
import qualified Blunt.Version as Version
import qualified Control.Concurrent as Concurrent
import qualified Control.Newtype as Newtype
import qualified Data.ByteString.Char8 as ByteString
import Data.Function ((&))
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp

data Server = Server
    { serverThreadId :: Concurrent.ThreadId
    } 

instance Common.Component Server where
    type Dependencies Server = (Environment.Environment, Logs.Logs, Metrics.Metrics, Wai.Application)
    start (environment,_logs,_metrics,application) = do
        let host = 
                environment & Environment.environmentServerHost &
                Newtype.unpack
        let port = Environment.environmentServerPort environment
        let serverName = "blunt-" ++ Version.versionString & ByteString.pack
        let settings = 
                Warp.defaultSettings & Warp.setHost host & Warp.setPort port &
                Warp.setServerName serverName
        threadId <- application & Warp.runSettings settings & Concurrent.forkIO
        return
            Server
            { serverThreadId = threadId
            }
    stop server = do
        server & serverThreadId & Concurrent.killThread
