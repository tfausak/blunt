{-# LANGUAGE OverloadedStrings #-}

module Blunt where

import Data.ByteString.Lazy (fromStrict)
import Network.HTTP.Types (ok200)
import Network.Wai (Application, queryString, responseLBS)
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = run 8080 application

application :: Application
application request respond = do
    let params = queryString request
        input = case lookup "input" params of
            Just (Just param) -> param
            _ -> ""
        status = ok200
        headers = [("Content-Type", "text/plain")]
        body = fromStrict input
        response = responseLBS status headers body
    respond response
