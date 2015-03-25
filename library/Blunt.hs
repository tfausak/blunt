{-# LANGUAGE OverloadedStrings #-}

module Blunt where

import Control.Exception (SomeException, evaluate, handle)
import Data.Aeson (ToJSON, (.=), encode, object, toJSON)
import Data.ByteString.Char8 (unpack)
import Data.ByteString.Lazy.Char8 (pack)
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
        body = pack html
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

html :: String
html = unlines
    [ "<!doctype html>"
    , ""
    , "<html>"
    , "  <head>"
    , "    <meta charset='utf-8'>"
    , "    <meta name='viewport' content='initial-scale = 1, maximum-scale = 1, minimum-scale = 1, width = device-width'>"
    , ""
    , "    <title>Blunt</title>"
    , ""
    , "    <style>"
    , css
    , "    </style>"
    , "  </head>"
    , ""
    , "  <body>"
    , "    <h1>Blunt</h1>"
    , ""
    , "    <dl>"
    , "      <dt>Input</dt>"
    , "      <dd>"
    , "        <input id='input' placeholder='sum xs = foldr (+) 0 xs' autocapitalize='none' autocomplete='off' autocorrect='off' autofocus spellcheck='false'>"
    , "      </dd>"
    , ""
    , "      <dt>Pointfree</dt>"
    , "      <dd>"
    , "        <div id='pointfree'></div>"
    , "      </dd>"
    , ""
    , "      <dt>Pointful</dt>"
    , "      <dd>"
    , "        <div id='pointful'></div>"
    , "      </dd>"
    , "    </dl>"
    , ""
    , "    <p>"
    , "      <a href='https://github.com/tfausak/blunt'>"
    , "        https://github.com/tfausak/blunt"
    , "      </a>"
    , "    </p>"
    , ""
    , "    <script>"
    , js
    , "    </script>"
    , "  </body>"
    , "</html>"
    ]

css :: String
css = unlines
    [ "html, body {"
    , "  background: #f5f5f5;"
    , "  color: #151515;"
    , "  font: 100%/1.5em sans-serif;"
    , "  margin: 0;"
    , "  padding: 0;"
    , "}"
    , ""
    , "body {"
    , "  box-sizing: border-box;"
    , "  margin: 0 auto;"
    , "  max-width: 40em;"
    , "  padding: 0 1.5em;"
    , "}"
    , ""
    , "h1 {"
    , "  color: #90a959;"
    , "  font-size: 2em;"
    , "  font-weight: bold;"
    , "  line-height: 3em;"
    , "  margin: 0;"
    , "  text-align: center;"
    , "}"
    , ""
    , "dl {"
    , "  margin: 0;"
    , "}"
    , ""
    , "dt {"
    , "  margin-top: 1.5em;"
    , "}"
    , ""
    , "dd {"
    , "  margin: 0;"
    , "}"
    , ""
    , "input, div {"
    , "  border: thin solid #e0e0e0;"
    , "  box-sizing: border-box;"
    , "  font-family: monospace;"
    , "  font-size: 1em;"
    , "  width: 100%;"
    , "}"
    , ""
    , "input {"
    , "  height: 3em;"
    , "  line-height: 3em;"
    , "  padding: 0 0.75em;"
    , "}"
    , ""
    , "div {"
    , "  padding: 0.75em;"
    , "  white-space: pre-wrap;"
    , "}"
    , ""
    , "p {"
    , "  margin: 1.5em 0 0 0;"
    , "  text-align: center;"
    , "}"
    ]

js :: String
js = unlines
    [ "'use strict';"
    , ""
    , "(function () {"
    , "  var input = document.getElementById('input');"
    , "  var pointfree = document.getElementById('pointfree');"
    , "  var pointful = document.getElementById('pointful');"
    , ""
    , "  var updateHash = function () {"
    , "    window.location.replace('#input=' + input.value);"
    , "  };"
    , ""
    , "  var updateOutput = function () {"
    , "    var request = new XMLHttpRequest();"
    , ""
    , "    request.onreadystatechange = function () {"
    , "      if (request.readyState === 4 && request.status === 200) {"
    , "        var response = JSON.parse(request.response);"
    , ""
    , "        pointfree.textContent = response.pointfree.join('\\n');"
    , "        pointful.textContent = response.pointful;"
    , "      }"
    , "    };"
    , "    request.open('GET', '/convert?input=' + encodeURIComponent(input.value));"
    , "    request.send();"
    , "  };"
    , ""
    , "  input.oninput = function (_event) {"
    , "    updateHash();"
    , "    updateOutput();"
    , "  };"
    , ""
    , "  if (window.location.hash.indexOf('#input=') === 0) {"
    , "    input.value = window.location.hash.substring(7);"
    , "    input.oninput();"
    , "  }"
    , "}());"
    ]
