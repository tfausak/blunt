{-# LANGUAGE OverloadedStrings #-}

module Blunt where

import Paths_blunt (getDataFileName)

import Data.ByteString.Char8 (unpack)
import Data.ByteString.Lazy (fromStrict)
import Data.ByteString.Lazy.Char8 (pack)
import Network.HTTP.Types (notFound404, ok200)
import Network.Wai (queryString, pathInfo, requestMethod, responseFile,
    responseLBS)
import Network.Wai.Handler.Warp (run)
import Pointfree (pointfree')

main :: IO ()
main = run 8080 $ \ request respond -> do
    index <- getDataFileName "index.html"
    let method = requestMethod request
        path = pathInfo request
        response = case (method, path) of
            ("GET", []) -> responseFile
                ok200
                [("Content-Type", "text/html; charset=utf-8")]
                index
                Nothing
            ("GET", ["pointfree"]) ->
                let params = queryString request
                    input = case lookup "input" params of
                        Just (Just param) -> param
                        _ -> ""
                    maybeOutput = pointfree' (unpack input)
                    body = case maybeOutput of
                        Just output -> pack output
                        Nothing -> fromStrict input
                in  responseLBS ok200 [("Content-Type", "text/plain; charset=utf-8")] body
            _ -> responseLBS notFound404 [] ""
    respond response
