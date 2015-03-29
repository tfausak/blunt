module Blunt.Application where

import Blunt.HTTP (httpApplication)
import Blunt.WebSocket (wsApplication)
import Network.Wai (Application)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets (defaultConnectionOptions)

application :: Application
application = websocketsOr
    defaultConnectionOptions wsApplication httpApplication
