{-# LANGUAGE OverloadedStrings #-}

module Blunt where

import Blunt.Markup (markup)

import Control.Exception (SomeException, evaluate, handle)
import Data.Aeson (ToJSON, (.=), encode, object, toJSON)
import Data.ByteString.Char8 (unpack)
import Lambdabot.Pointful (pointful)
import Network.HTTP.Types (notFound404, ok200)
import Network.Wai (Application, Request, Response, queryString, pathInfo,
    requestMethod, responseLBS)
import Network.Wai.Handler.Warp (runEnv)
import Pointfree (pointfree)

main :: IO ()
main = runEnv 8080 application

application :: Application
application request respondWith = do
    let action = route request
    response <- action request
    respondWith response

type Action = Request -> IO Response

route :: Request -> Action
route request = case (requestMethod request, pathInfo request) of
    ("GET", []) -> indexAction
    ("GET", ["convert"]) -> convertAction
    _ -> notFoundAction

indexAction :: Action
indexAction _request = do
    let headers = [("Content-Type", "text/html")]
        body = markup
    return (responseLBS ok200 headers body)

data Result = Result
    { resultInput :: String
    , resultPointfree :: [String]
    , resultPointful :: String
    } deriving (Read, Show)

instance ToJSON Result where
    toJSON result = object
        [ "input" .= resultInput result
        , "pointfree" .= resultPointfree result
        , "pointful" .= resultPointful result
        ]

convertAction :: Action
convertAction request = do
    let input = case lookup "input" (queryString request) of
            Just (Just param) -> unpack param
            _ -> ""

    pf <- safePointfree input
    let pl = pointful input
        result = Result
            { resultInput = input
            , resultPointfree = pf
            , resultPointful = pl
            }

    let headers = [("Content-Type", "application/json")]
        body = encode result
    return (responseLBS ok200 headers body)

notFoundAction :: Action
notFoundAction _request = return (responseLBS notFound404 [] "")

safePointfree :: String -> IO [String]
safePointfree = handle handler . evaluate . pointfree where
    handler :: SomeException -> IO [String]
    handler _ = return []
