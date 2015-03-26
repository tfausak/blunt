module Blunt.Pointful where

import Data.List (isPrefixOf)
import Lambdabot.Pointful (pointful)

safePointful :: String -> Maybe String
safePointful input =
    let output = pointful input
    in  if any (`isPrefixOf` output) ["Error:", "<unknown>.hs:"]
        then Nothing
        else Just output
