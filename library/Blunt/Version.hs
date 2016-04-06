module Blunt.Version where

import Data.Function ((&))
import qualified Data.String as String
import qualified Data.Version as Version
import qualified Paths_blunt as This

version :: Version.Version
version = This.version

versionString
    :: (String.IsString string)
    => string
versionString = version & Version.showVersion & String.fromString
