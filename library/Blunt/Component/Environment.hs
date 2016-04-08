{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module Blunt.Component.Environment where

import qualified Blunt.Component.Common as Common
import qualified Control.Newtype as Newtype
import qualified Data.ByteString.Char8 as ByteString
import Data.Function ((&))
import qualified Data.String as String
import qualified GHC.Generics as Generics
import qualified Network.Wai.Handler.Warp as Warp
import qualified System.Envy as Envy
import qualified System.Log.Logger as Log
import qualified Text.Read as Read

data Environment = Environment
    { environmentHoneybadgerApiKey :: String
    , environmentLogsPriority :: Priority
    , environmentMetricsHost :: ByteString.ByteString
    , environmentMetricsPort :: Int
    , environmentServerHost :: Host
    , environmentServerPort :: Int
    } deriving (Generics.Generic)

instance Common.Component Environment where
    type Dependencies Environment = ()
    start () = do
        result <- Envy.decodeEnv
        case result of
            Left message -> fail message
            Right environment -> return environment

instance Envy.DefConfig Environment where
    defConfig =
        Environment
        { environmentHoneybadgerApiKey = ""
        , environmentLogsPriority = Newtype.pack Log.NOTICE
        , environmentMetricsHost = ByteString.pack "127.0.0.1"
        , environmentMetricsPort = 8081
        , environmentServerHost = "127.0.0.1" & String.fromString &
          Newtype.pack
        , environmentServerPort = 8080
        }

instance Envy.FromEnv Environment where
    fromEnv =
        Envy.gFromEnvCustom
            Envy.Option
            { Envy.customPrefix = ""
            , Envy.dropPrefixCount = length "Environment"
            }

newtype Host =
    Host Warp.HostPreference
    deriving (Generics.Generic)

instance Newtype.Newtype Host

instance Envy.Var Host where
    fromVar var = var & String.fromString & Newtype.pack & return
    toVar host = host & Newtype.unpack & show

newtype Priority =
    Priority Log.Priority
    deriving (Generics.Generic)

instance Newtype.Newtype Priority

instance Envy.Var Priority where
    fromVar var = do
        priority <- Read.readMaybe var
        priority & Newtype.pack & return
    toVar priority = priority & Newtype.unpack & show
