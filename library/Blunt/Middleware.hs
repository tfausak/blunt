module Blunt.Middleware where

import Network.Wai (Middleware)

middleware :: Middleware
middleware = id
