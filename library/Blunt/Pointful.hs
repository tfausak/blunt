module Blunt.Pointful where

import Data.List (isPrefixOf, isSuffixOf)
import Lambdabot.Pointful (pointful)

safePointful :: String -> Maybe String
safePointful input =
    let output = pointful input
    in  if any (`isPrefixOf` output) ["Error:", "<unknown>.hs:"]
        then Nothing
        else if ";" `isSuffixOf` output && not (";" `isSuffixOf` input)
            then Just (init output)
            else Just output
