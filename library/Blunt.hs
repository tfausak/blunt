{-# LANGUAGE OverloadedStrings #-}

module Blunt where

import Control.Exception (SomeException, evaluate, handle)
import Data.ByteString.Char8 (unpack)
import Data.ByteString.Lazy (fromStrict)
import Data.ByteString.Lazy.Char8 (pack)
import Network.HTTP.Types (notFound404, ok200)
import Network.Wai (Application, Request, Response, queryString, pathInfo,
    requestMethod, responseLBS)
import Network.Wai.Handler.Warp (runEnv)
import Pointfree (pointfree')

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
    ("GET", ["pointfree"]) -> pointfreeAction
    _ -> notFoundAction

indexAction :: Action
indexAction _request = do
    let headers = [("Content-Type", "text/html; charset=utf-8")]
        body = pack html
    return (responseLBS ok200 headers body)

pointfreeAction :: Action
pointfreeAction request = do
    let params = queryString request
        input = case lookup "input" params of
            Just (Just param) -> param
            _ -> ""
    maybeOutput <- safePointfree (unpack input)
    let headers = [("Content-Type", "text/plain; charset=utf-8")]
        body = case maybeOutput of
            Just output -> pack output
            Nothing -> fromStrict input
    return (responseLBS ok200 headers body)

notFoundAction :: Action
notFoundAction _request = return (responseLBS notFound404 [] "")

safePointfree :: String -> IO (Maybe String)
safePointfree = handle handler . evaluate . pointfree' where
    handler :: SomeException -> IO (Maybe String)
    handler _ = return Nothing

html :: String
html = unlines
    [ "<!doctype html>"
    , ""
    , "<html>"
    , "  <head>"
    , "    <meta name='viewport' content='initial-scale = 1, width = device-width'>"
    , ""
    , "    <title>Blunt</title>"
    , "  </head>"
    , ""
    , "  <body>"
    , "    <h1>Blunt</h1>"
    , ""
    , "    <dl>"
    , "      <dt>Input</dt>"
    , "      <dd>"
    , "        <input id='input' placeholder='sum xs = foldr (+) 0 xs' autofocus>"
    , "      </dd>"
    , ""
    , "      <dt>Output</dt>"
    , "      <dd>"
    , "        <input id='output' placeholder='sum = foldr (+) 0' readonly>"
    , "      </dd>"
    , "    </dl>"
    , ""
    , "    <script>"
    , js
    , "    </script>"
    , "  </body>"
    , "</html>"
    ]

js :: String
js = unlines
    [ "'use strict';"
    , ""
    , "(function () {"
    , "  var input = document.getElementById('input');"
    , "  var output = document.getElementById('output');"
    , ""
    , "  input.oninput = function (_event) {"
    , "    var request = new XMLHttpRequest();"
    , ""
    , "    request.onreadystatechange = function () {"
    , "      if (request.readyState === 4 && request.status === 200) {"
    , "        output.value = request.response;"
    , "      }"
    , "    };"
    , "    request.open('GET', '/pointfree?input=' + encodeURIComponent(input.value));"
    , "    request.send();"
    , "  };"
    , "}());"
    ]
