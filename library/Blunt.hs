module Blunt where

import Data.ByteString.Lazy (empty)
import Network.HTTP.Types (ok200)
import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = run 8080 application

application :: Application
application _request respond =
    let status = ok200
        headers = []
        body = empty
        response = responseLBS status headers body
    in  respond response
