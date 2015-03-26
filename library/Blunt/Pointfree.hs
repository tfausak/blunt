module Blunt.Pointfree where

import Control.Exception (SomeException, evaluate, handle)
import Pointfree (pointfree)

safePointfree :: String -> IO [String]
safePointfree = handle handler . evaluate . pointfree where
    handler :: SomeException -> IO [String]
    handler _ = return []
