{-# LANGUAGE TypeFamilies #-}

module Blunt.Component.Logs where

import qualified Blunt.Component.Common as Common
import qualified Blunt.Component.Environment as Environment
import qualified Control.Newtype as Newtype
import Data.Function ((&))
import qualified System.IO as IO
import qualified System.Log.Handler as Handler
import qualified System.Log.Handler.Simple as Handler
import qualified System.Log.Formatter as Formatter
import qualified System.Log.Logger as Log

data Logs = Logs
    { logsLogger :: Log.Logger
    , logsOriginalLogger :: Log.Logger
    } 

instance Common.Component Logs where
    type Dependencies Logs = (Environment.Environment)
    start (environment) = do
        originalLogger <- Log.getRootLogger
        let priority = 
                environment & Environment.environmentLogsPriority &
                Newtype.unpack
        basicHandler <- Handler.streamHandler IO.stdout Log.DEBUG
        let formatter = 
                Formatter.tfLogFormatter "%FT%T%Q%z" "$time - $prio - $msg"
        let handler = Handler.setFormatter basicHandler formatter
        let logger = 
                originalLogger & Log.setLevel priority &
                Log.setHandlers [handler]
        Log.saveGlobalLogger logger
        return
            Logs
            { logsLogger = logger
            , logsOriginalLogger = originalLogger
            }
    stop logs = do
        logs & logsOriginalLogger & Log.saveGlobalLogger
