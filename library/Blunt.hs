{-# LANGUAGE OverloadedStrings #-}

module Blunt where

import Control.Exception (SomeException, evaluate, handle)
import Data.ByteString.Char8 (unpack)
import Data.ByteString.Lazy (fromStrict)
import Data.ByteString.Lazy.Char8 (pack)
import Network.HTTP.Types (notFound404, ok200)
import Network.Wai (queryString, pathInfo, requestMethod, responseLBS)
import Network.Wai.Handler.Warp (run)
import Pointfree (pointfree')

main :: IO ()
main = run 8080 $ \ request respond -> do
    let method = requestMethod request
        path = pathInfo request
    response <- case (method, path) of
            ("GET", []) -> return $ responseLBS
                ok200
                [("Content-Type", "text/html; charset=utf-8")]
                (pack html)
            ("GET", ["pointfree"]) -> do
                let params = queryString request
                    input = case lookup "input" params of
                        Just (Just param) -> param
                        _ -> ""
                maybeOutput <- safePointfree (unpack input)
                let body = case maybeOutput of
                        Just output -> pack output
                        Nothing -> fromStrict input
                return $ responseLBS ok200 [("Content-Type", "text/plain; charset=utf-8")] body
            _ -> return $ responseLBS notFound404 [] ""
    respond response

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
