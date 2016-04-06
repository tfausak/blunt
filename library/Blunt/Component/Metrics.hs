{-# LANGUAGE TypeFamilies #-}

module Blunt.Component.Metrics where

import qualified Blunt.Component.Common as Common
import qualified Blunt.Component.Environment as Environment
import qualified Control.Concurrent as Concurrent
import qualified Data.ByteString as ByteString
import Data.Function ((&))
import qualified Data.IORef as IORef
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified System.Metrics as Metrics
import qualified System.Metrics.Counter as Counter
import qualified System.Metrics.Distribution as Distribution
import qualified System.Metrics.Gauge as Gauge
import qualified System.Remote.Monitoring as Monitoring
import qualified System.TimeIt as TimeIt

data Metrics = Metrics
    { metricsCounters :: IORef.IORef (Map.Map Text.Text Counter.Counter)
    , metricsDistributions :: IORef.IORef (Map.Map Text.Text Distribution.Distribution)
    , metricsGauges :: IORef.IORef (Map.Map Text.Text Gauge.Gauge)
    , metricsServer :: Maybe Monitoring.Server
    , metricsStore :: Metrics.Store
    }

instance Common.Component Metrics where
    type Dependencies Metrics = (Environment.Environment)
    start (environment) = do
        store <- Metrics.newStore
        Metrics.registerGcMetrics store
        counters <- IORef.newIORef Map.empty
        distributions <- IORef.newIORef Map.empty
        gauges <- IORef.newIORef Map.empty
        let host = Environment.environmentMetricsHost environment
        maybeServer <-
            if ByteString.null host
                then return Nothing
                else do
                    let port = Environment.environmentMetricsPort environment
                    server <- Monitoring.forkServerWith store host port
                    server & Just & return
        return
            Metrics
            { metricsCounters = counters
            , metricsDistributions = distributions
            , metricsGauges = gauges
            , metricsServer = maybeServer
            , metricsStore = store
            }
    stop metrics = do
        case metricsServer metrics of
            Nothing -> return ()
            Just server ->
                server & Monitoring.serverThreadId & Concurrent.killThread

metricsCounter :: Metrics -> String -> Int -> IO ()
metricsCounter metrics stringName value = do
    let name = Text.pack stringName
    let ref = metricsCounters metrics
    counters <- IORef.readIORef ref
    counter <-
        case Map.lookup name counters of
            Nothing -> do
                counter <- Metrics.createCounter name (metricsStore metrics)
                IORef.modifyIORef ref (Map.insert name counter)
                return counter
            Just counter -> return counter
    Counter.add counter (fromIntegral value)

metricsDistribution :: Metrics -> String -> Double -> IO ()
metricsDistribution metrics stringName value = do
    let name = Text.pack stringName
    let ref = metricsDistributions metrics
    distributions <- IORef.readIORef ref
    distribution <-
        case Map.lookup name distributions of
            Nothing -> do
                distribution <-
                    Metrics.createDistribution name (metricsStore metrics)
                IORef.modifyIORef ref (Map.insert name distribution)
                return distribution
            Just distribution -> return distribution
    Distribution.add distribution value

metricsGauge :: Metrics -> String -> Int -> IO ()
metricsGauge metrics stringName value = do
    let name = Text.pack stringName
    let ref = metricsGauges metrics
    gauges <- IORef.readIORef ref
    gauge <-
        case Map.lookup name gauges of
            Nothing -> do
                gauge <- Metrics.createGauge name (metricsStore metrics)
                IORef.modifyIORef ref (Map.insert name gauge)
                return gauge
            Just gauge -> return gauge
    Gauge.add gauge (fromIntegral value)

metricsTimed :: Metrics -> String -> IO a -> IO a
metricsTimed metrics name action = do
    (time,result) <- TimeIt.timeItT action
    metricsDistribution metrics name time
    return result
