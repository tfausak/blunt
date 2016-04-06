{-# LANGUAGE TypeFamilies #-}

module Blunt.Component.Metrics where

import qualified Blunt.Component.Common as Common
import qualified Blunt.Component.Environment as Environment
import qualified Control.Concurrent as Concurrent
import qualified Data.ByteString as ByteString
import Data.Function ((&))
import qualified System.Remote.Monitoring as Monitoring

data Metrics = Metrics
    { metricsServer :: Maybe Monitoring.Server
    } 

instance Common.Component Metrics where
    type Dependencies Metrics = (Environment.Environment)
    start (environment) = do
        let host = Environment.environmentMetricsHost environment
        maybeServer <- 
            if ByteString.null host
                then return Nothing
                else do
                    let port = Environment.environmentMetricsPort environment
                    server <- Monitoring.forkServer host port
                    server & Just & return
        return
            Metrics
            { metricsServer = maybeServer
            }
    stop metrics = do
        case metricsServer metrics of
            Nothing -> return ()
            Just server -> 
                server & Monitoring.serverThreadId & Concurrent.killThread
