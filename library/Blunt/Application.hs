module Blunt.Application where

import Blunt.Middleware (middleware)
import Blunt.Router (route)
import Control.Monad (forever)
import Network.Wai (Application)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets (ServerApp, acceptRequest, defaultConnectionOptions,
    receiveDataMessage, sendDataMessage)

application :: Application
application = websocketsOr
    defaultConnectionOptions
    wsApplication
    (middleware httpApplication)

wsApplication :: ServerApp
wsApplication pending = do
    connection <- acceptRequest pending
    forever $ do
        message <- receiveDataMessage connection
        print message
        sendDataMessage connection message

httpApplication :: Application
httpApplication request respondWith = do
    let action = route request
    response <- action request
    respondWith response
