module Blunt.Middleware where

import Network.Wai (Middleware)
import Network.Wai.Middleware.Gzip (def, gzip)

middleware :: Middleware
middleware = gzip def
