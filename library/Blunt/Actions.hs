{-# LANGUAGE OverloadedStrings #-}

module Blunt.Actions where

import Blunt.Markup (markup)
import Data.Aeson (ToJSON, object, toJSON, (.=))
import Network.HTTP.Types (notFound404, ok200)
import Network.Wai (Request, Response, responseLBS)

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

notFoundAction :: Request -> IO Response
notFoundAction _request = return (responseLBS notFound404 [] "")
