{-# LANGUAGE OverloadedStrings #-}

module Blunt.App.Markup where

import qualified Blunt.App.Script as Script
import qualified Blunt.App.Style as Style
import qualified Blunt.Version as Version
import qualified Data.ByteString.Lazy as ByteString
import Lucid

markup :: ByteString.ByteString
markup = renderBS html

html :: Html ()
html =
    doctypehtml_ $
    do head_ $
           do meta_
                  [ name_ "viewport"
                  , content_ "initial-scale = 1, width = device-width"]
              title_ "Online Haskell pointfree and pointful converter"
              style_ [] Style.style
       body_ $
           do h1_ "Pointfree \8596\65038 Pointful"
              dl_ $
                  do dt_ "Input"
                     dd_ $
                         do input_
                                [ id_ "input"
                                , placeholder_ "sum xs = foldr (+) 0 xs"
                                , autocomplete_ "off"
                                , autofocus_
                                , spellcheck_ "off"
                                , term "autocapitalize" "none"
                                , term "autocorrect" "off"]
                     dt_ "Pointfree"
                     dd_ (div_ [class_ "output", id_ "pointfree"] "")
                     dt_ "Pointful"
                     dd_ (div_ [class_ "output", id_ "pointful"] "")
              p_ $
                  do "This page converts Haskell expressions between the pointfree and pointful styles. It is a web front end to the "
                     a_
                         [href_ "https://hackage.haskell.org/package/pointfree"]
                         "pointfree"
                     " and "
                     a_
                         [href_ "https://hackage.haskell.org/package/pointful"]
                         "pointful"
                     " libraries. Read more about the pointfree style on "
                     a_
                         [href_ "https://wiki.haskell.org/Pointfree"]
                         "the Haskell wiki"
                     "."
              p_ $
                  do "Powered by "
                     a_ [href_ "https://github.com/tfausak/blunt"] "Blunt"
                     " version "
                     Version.versionString
                     ". Created by "
                     a_ [href_ "http://taylor.fausak.me"] "Taylor Fausak"
                     "."
              script_ [] Script.script
