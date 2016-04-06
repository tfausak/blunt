module Blunt.Main where

import qualified Blunt.App as App
import qualified Blunt.Component as Component
import qualified Control.Concurrent as Concurrent
import qualified System.Signal as Signal

main :: IO ()
main = do
    system <- Component.startSystem (App.application)
    sentinel <- Concurrent.newEmptyMVar
    let handler _signal = Concurrent.putMVar sentinel ()
    Signal.installHandler Signal.sigINT handler
    Concurrent.takeMVar sentinel
    Component.stop system
