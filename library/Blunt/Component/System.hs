{-# LANGUAGE TypeFamilies #-}

module Blunt.Component.System where

import qualified Blunt.Component.Common as Common
import qualified Blunt.Component.Environment as Environment
import qualified Blunt.Component.Logs as Logs
import qualified Blunt.Component.Metrics as Metrics
import qualified Blunt.Component.Server as Server
import Data.Function ((&))
import qualified Network.Wai as Wai

data System = System
    { systemEnvironment :: Environment.Environment
    , systemLogs :: Logs.Logs
    , systemMetrics :: Metrics.Metrics
    , systemServer :: Server.Server
    } 

instance Common.Component System where
    type Dependencies System = (Wai.Application)
    start (application) = do
        putStrLn "Starting system..."
        putStrLn "Starting environment..."
        environment <- Common.start ()
        putStrLn "Starting logs..."
        logs <- Common.start (environment)
        putStrLn "Starting metrics..."
        metrics <- Common.start (environment)
        putStrLn "Starting server..."
        server <- Common.start (environment, logs, metrics, application)
        putStrLn "Started system."
        return
            System
            { systemEnvironment = environment
            , systemLogs = logs
            , systemMetrics = metrics
            , systemServer = server
            }
    stop system = do
        putStrLn "Stopping system..."
        putStrLn "Stopping server..."
        system & systemServer & Common.stop
        putStrLn "Stopping metrics..."
        system & systemMetrics & Common.stop
        putStrLn "Stopping logs..."
        system & systemEnvironment & Common.stop
        putStrLn "Stopping environment..."
        system & systemEnvironment & Common.stop
        putStrLn "Stopped system."

startSystem :: Common.Dependencies System -> IO System
startSystem dependencies = Common.start dependencies
