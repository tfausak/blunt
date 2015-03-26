{-# LANGUAGE OverloadedStrings #-}

module Blunt.Actions where

import Blunt.Markup (markup)
import Blunt.Pointfree (safePointfree)
import Blunt.Pointful (safePointful)

import Data.Aeson (ToJSON, (.=), encode, object, toJSON)
import Data.ByteString.Char8 (unpack)
import Network.HTTP.Types (notFound404, ok200)
import Network.Wai (Request, Response, queryString, responseLBS)

indexAction :: Request -> IO Response
indexAction _request = do
    let headers = [("Content-Type", "text/html")]
        body = markup
    return (responseLBS ok200 headers body)

data Result = Result
    { resultInput :: String
    , resultPointfree :: [String]
    , resultPointful :: Maybe String
    } deriving (Read, Show)

instance ToJSON Result where
    toJSON result = object
        [ "input" .= resultInput result
        , "pointfree" .= resultPointfree result
        , "pointful" .= resultPointful result
        ]

convertAction :: Request -> IO Response
convertAction request = do
    let input = case lookup "input" (queryString request) of
            Just (Just param) -> unpack param
            _ -> ""

    pf <- safePointfree input
    let pl = safePointful input
        result = Result
            { resultInput = input
            , resultPointfree = pf
            , resultPointful = pl
            }

    let headers = [("Content-Type", "application/json")]
        body = encode result
    return (responseLBS ok200 headers body)

notFoundAction :: Request -> IO Response
notFoundAction _request = return (responseLBS notFound404 [] "")
