{-# LANGUAGE TypeFamilies #-}

module Blunt.Component.System where

import qualified Bento
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

instance Bento.Component System where
    type Dependencies System = ((Logs.Logs, Metrics.Metrics) -> Wai.Application)
    start (makeApplication) = do
        putStrLn "Starting system..."
        putStrLn "Starting environment..."
        environment <- Bento.start ()
        putStrLn "Starting logs..."
        logs <- Bento.start (environment)
        putStrLn "Starting metrics..."
        metrics <- Bento.start (environment)
        putStrLn "Starting server..."
        server <- Bento.start (environment, logs, metrics, makeApplication)
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
        system & systemServer & Bento.stop
        putStrLn "Stopping metrics..."
        system & systemMetrics & Bento.stop
        putStrLn "Stopping logs..."
        system & systemEnvironment & Bento.stop
        putStrLn "Stopping environment..."
        system & systemEnvironment & Bento.stop
        putStrLn "Stopped system."

startSystem :: Bento.Dependencies System -> IO System
startSystem dependencies = Bento.start dependencies
