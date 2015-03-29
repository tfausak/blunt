{-# LANGUAGE OverloadedStrings #-}

module Blunt.WebSocket where

import Blunt.Pointfree (safePointfree)
import Blunt.Pointful (safePointful)
import Control.Monad (forever)
import Data.Aeson (ToJSON, encode, object, toJSON, (.=))
import Data.ByteString.Lazy.Char8 (unpack)
import Network.WebSockets (ServerApp, acceptRequest, receiveData, sendTextData)

wsApplication :: ServerApp
wsApplication pending = do
    connection <- acceptRequest pending
    forever $ do
        message <- receiveData connection
        let input = unpack message

        pf <- safePointfree input
        let pl = safePointful input
            result = Result
                { resultInput = input
                , resultPointfree = pf
                , resultPointful = pl
                }

        sendTextData connection (encode result)

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
