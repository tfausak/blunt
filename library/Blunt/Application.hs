module Blunt.Application where

import Blunt.Router (route)

import Network.Wai (Application)

application :: Application
application request respondWith = do
    let action = route request
    response <- action request
    respondWith response
