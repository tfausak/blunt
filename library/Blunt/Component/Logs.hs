{-# LANGUAGE TypeFamilies #-}

module Blunt.Component.Logs where

import qualified Bento
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

instance Bento.Component Logs where
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

logsDebug :: Logs -> String -> IO ()
logsDebug logs message = Log.logL (logsLogger logs) Log.DEBUG message

logsInfo :: Logs -> String -> IO ()
logsInfo logs message = Log.logL (logsLogger logs) Log.INFO message

logsNotice :: Logs -> String -> IO ()
logsNotice logs message = Log.logL (logsLogger logs) Log.NOTICE message

logsWarning :: Logs -> String -> IO ()
logsWarning logs message = Log.logL (logsLogger logs) Log.WARNING message

logsError :: Logs -> String -> IO ()
logsError logs message = Log.logL (logsLogger logs) Log.ERROR message

logsCritical :: Logs -> String -> IO ()
logsCritical logs message = Log.logL (logsLogger logs) Log.CRITICAL message

logsAlert :: Logs -> String -> IO ()
logsAlert logs message = Log.logL (logsLogger logs) Log.ALERT message

logsEmergency :: Logs -> String -> IO ()
logsEmergency logs message = Log.logL (logsLogger logs) Log.EMERGENCY message
