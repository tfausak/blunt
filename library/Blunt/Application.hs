module Blunt.Application where

import Blunt.Actions
import Blunt.Pointfree
import Blunt.Pointful
import Data.Aeson
import Data.ByteString.Lazy.Char8

import Blunt.Middleware (middleware)
import Blunt.Router (route)
import Control.Monad (forever)
import Network.Wai (Application)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets (ServerApp, acceptRequest, defaultConnectionOptions,
    receiveData, sendTextData)

application :: Application
application = websocketsOr
    defaultConnectionOptions
    wsApplication
    (middleware httpApplication)

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

httpApplication :: Application
httpApplication request respondWith = do
    let action = route request
    response <- action request
    respondWith response
