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
        Log.updateGlobalLogger Log.rootLoggerName Log.removeHandler
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
        return
            Logs
            { logsLogger = logger
            , logsOriginalLogger = originalLogger
            }
    stop logs = do
        logs & logsOriginalLogger & Log.saveGlobalLogger

logsDebug :: String -> Logs -> IO ()
logsDebug message logs = Log.logL (logsLogger logs) Log.DEBUG message

logsInfo :: String -> Logs -> IO ()
logsInfo message logs = Log.logL (logsLogger logs) Log.INFO message

logsNotice :: String -> Logs -> IO ()
logsNotice message logs = Log.logL (logsLogger logs) Log.NOTICE message

logsWarning :: String -> Logs -> IO ()
logsWarning message logs = Log.logL (logsLogger logs) Log.WARNING message

logsError :: String -> Logs -> IO ()
logsError message logs = Log.logL (logsLogger logs) Log.ERROR message

logsCritical :: String -> Logs -> IO ()
logsCritical message logs = Log.logL (logsLogger logs) Log.CRITICAL message

logsAlert :: String -> Logs -> IO ()
logsAlert message logs = Log.logL (logsLogger logs) Log.ALERT message

logsEmergency :: String -> Logs -> IO ()
logsEmergency message logs = Log.logL (logsLogger logs) Log.EMERGENCY message
