module Blunt.Application where

import Blunt.Middleware (middleware)
import Blunt.Router (route)
import Network.Wai (Application)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets (ServerApp, defaultConnectionOptions)

application :: Application
application = websocketsOr
    defaultConnectionOptions
    wsApplication
    (middleware httpApplication)

wsApplication :: ServerApp
wsApplication _pending = return ()

httpApplication :: Application
httpApplication request respondWith = do
    let action = route request
    response <- action request
    respondWith response
