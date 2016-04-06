{-# LANGUAGE TypeFamilies #-}

module Blunt.Component.Common where

class Component component  where
    type Dependencies component :: *
    start :: Dependencies component -> IO component
    stop :: component -> IO ()
    stop _component = return ()
