module Blunt.Application where

import Blunt.Middleware (middleware)
import Blunt.Router (route)
import Network.Wai (Application)

application :: Application
application = middleware httpApplication

httpApplication :: Application
httpApplication request respondWith = do
    let action = route request
    response <- action request
    respondWith response
