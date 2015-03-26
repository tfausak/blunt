module Blunt.Main where

import Blunt.Application (application)
import Network.Wai.Handler.Warp (runEnv)

main :: IO ()
main = runEnv 8080 application
