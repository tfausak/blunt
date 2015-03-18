{-# LANGUAGE OverloadedStrings #-}

module Blunt where

import Data.ByteString.Char8 (unpack)
import Data.ByteString.Lazy (fromStrict)
import Data.ByteString.Lazy.Char8 (pack)
import Network.HTTP.Types (ok200)
import Network.Wai (Application, queryString, responseLBS)
import Network.Wai.Handler.Warp (run)
import Pointfree (pointfree')

main :: IO ()
main = run 8080 application

application :: Application
application request respond = do
    let params = queryString request
        input = case lookup "input" params of
            Just (Just param) -> param
            _ -> ""
        maybeOutput = pointfree' (unpack input)
        status = ok200
        headers = [("Content-Type", "text/plain")]
        body = case maybeOutput of
            Just output -> pack output
            Nothing -> fromStrict input
        response = responseLBS status headers body
    respond response
