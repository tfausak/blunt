module Blunt.Middleware where

import Network.Wai (Middleware)
import Network.Wai.Middleware.Gzip (def, gzip)
import Network.Wai.Middleware.RequestLogger (logStdout)

middleware :: Middleware
middleware = gzip def . logStdout
