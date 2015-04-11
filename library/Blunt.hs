{-# LANGUAGE OverloadedStrings #-}

module Blunt where

import Flow

import Blunt.Markup (markup)
import Control.Exception (SomeException, evaluate, handle)
import Control.Monad (forever)
import Data.Aeson (ToJSON, encode, object, toJSON, (.=))
import Data.List (isPrefixOf, isSuffixOf)
import Data.Text.Lazy (Text, unpack)
import Lambdabot.Pointful (pointful)
import Network.HTTP.Types (notFound404, ok200)
import Network.Wai (Application, pathInfo, requestMethod, responseLBS)
import Network.Wai.Handler.Warp (runEnv)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.Wai.Middleware.Gzip (def, gzip)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Network.WebSockets (ServerApp, acceptRequest, defaultConnectionOptions,
    forkPingThread, receiveData, sendTextData)
import Pointfree (pointfree)

main :: IO ()
main = runEnv 8080 application

application :: Application
application = websocketsOr defaultConnectionOptions ws http

ws :: ServerApp
ws pending = do
    connection <- acceptRequest pending
    forkPingThread connection 30
    forever <| do
        message <- receiveData connection
        result <- convert message
        sendTextData connection (encode result)

http :: Application
http = logStdout .> gzip def <| \ request respond ->
    respond <| case (requestMethod request, pathInfo request) of
        ("GET", []) -> responseLBS status headers body where
            status = ok200
            headers = [("Content-Type", "text/html; charset=utf-8")]
            body = markup
        _ -> responseLBS notFound404 [] ""

convert :: Text -> IO Conversion
convert message = do
    let input = unpack message
    pf <- safePointfree input
    let pl = safePointful input
    return Conversion
        { conversionPointfree = pf
        , conversionPointful = pl
        }

safePointfree :: String -> IO [String]
safePointfree = pointfree .> evaluate .> handle handler

handler :: SomeException -> IO [String]
handler _ = return []

safePointful :: String -> Maybe String
safePointful input =
    let output = pointful input
    in  if any (`isPrefixOf` output) ["Error:", "<unknown>.hs:"]
        then Nothing
        else if ";" `isSuffixOf` output && not (";" `isSuffixOf` input)
            then Just (init output)
            else Just output

data Conversion = Conversion
    { conversionPointfree :: [String]
    , conversionPointful :: Maybe String
    } deriving (Read, Show)

instance ToJSON Conversion where
    toJSON result = object
        [ "pointfree" .= conversionPointfree result
        , "pointful" .= conversionPointful result
        ]
