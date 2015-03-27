module Blunt.Main where

import Blunt.Application (application)
import Blunt.Middleware (middleware)
import Network.Wai.Handler.Warp (runEnv)

main :: IO ()
main = runEnv 8080 (middleware application)
