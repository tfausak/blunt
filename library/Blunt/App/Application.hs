{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Blunt.App.Application where

import qualified Blunt.App.Markup as Markup
import qualified Blunt.Component as Component
import Control.Category ((>>>))
import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Lazy.Char8 as ByteString
import Data.Function ((&))
import qualified Data.List as List
import qualified Data.Text.Lazy as Text
import qualified GHC.Generics as Generics
import qualified Lambdabot.Pointful as Pointful
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.WebSockets as WebSockets
import qualified Network.WebSockets as WebSockets
import qualified Pointfree as Pointfree
import qualified System.Timeout as Timeout

application :: (Component.Logs, Component.Metrics) -> Wai.Application
application (logs,metrics) =
    WebSockets.websocketsOr
        WebSockets.defaultConnectionOptions
        (ws logs metrics)
        http

ws :: Component.Logs -> Component.Metrics -> WebSockets.ServerApp
ws logs metrics pending = do
    connection <- WebSockets.acceptRequest pending
    WebSockets.forkPingThread connection 30
    app connection & Monad.forever
  where
    app connection = do
        Component.metricsCounter metrics "server.convert" 1
        Component.metricsTimed
            metrics
            "server.convert_duration_s"
            (do message <- WebSockets.receiveData connection
                Component.logsInfo logs (Text.unpack message)
                result <- safeConvert message
                let json = Aeson.encode result
                Component.logsDebug logs (ByteString.unpack json)
                json & WebSockets.sendTextData connection)

http :: Wai.Application
http request respond = do
    let response =
            case (Wai.requestMethod request, Wai.pathInfo request) of
                ("GET",[]) -> Wai.responseLBS status headers body
                    where status = HTTP.ok200
                          headers =
                              [("Content-Type", "text/html; charset=utf-8")]
                          body = Markup.markup
                _ -> Wai.responseLBS HTTP.notFound404 [] ""
    respond response

safeConvert :: Text.Text -> IO Conversion
safeConvert message = do
    result <- Timeout.timeout 100000 (convert message)
    case result of
        Nothing ->
            return
                Conversion
                { conversionPointfree = []
                , conversionPointful = []
                }
        Just conversion -> return conversion

convert :: Text.Text -> IO Conversion
convert message = do
    let input = Text.unpack message
    pf <- safePointfree input
    let pl = safePointful input
    return
        Conversion
        { conversionPointfree = pf
        , conversionPointful = pl
        }

safePointfree :: String -> IO [String]
safePointfree input =
    input & Pointfree.pointfree & Exception.evaluate & Exception.handle handler

handler :: Exception.SomeException -> IO [String]
handler _exception = return []

safePointful :: String -> [String]
safePointful input =
    let output = Pointful.pointful input
    in if any (`List.isPrefixOf` output) ["Error:", "<unknown>.hs:"]
           then []
           else if ";" `List.isSuffixOf` output &&
                   not (";" `List.isSuffixOf` input)
                    then [init output]
                    else [output]

data Conversion = Conversion
    { conversionPointfree :: [String]
    , conversionPointful :: [String]
    } deriving (Generics.Generic,Read,Show)

instance Aeson.ToJSON Conversion where
    toJSON =
        Aeson.genericToJSON
            Aeson.defaultOptions
            { Aeson.fieldLabelModifier = drop (length ("Conversion" :: String)) >>>
              Aeson.camelTo2 '_'
            }
