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
    doctypehtml_
        (do htmlHead
            htmlBody)

htmlHead :: Html ()
htmlHead = do
    meta_
        [name_ "viewport", content_ "initial-scale = 1, width = device-width"]
    title_ "pointfree.info: Online Haskell pointfree and pointful converter"
    style_ [] Style.style

htmlBody :: Html ()
htmlBody = do
    h1_ "pointfree.info"
    dl_
        (do dt_ "Input"
            dd_
                (input_
                     [ autocomplete_ "off"
                     , autofocus_
                     , id_ "input"
                     , placeholder_ "sum xs = foldr (+) 0 xs"
                     , spellcheck_ "off"
                     , term "autocapitalize" "none"
                     , term "autocorrect" "off"])
            dt_ "Pointfree"
            dd_ (div_ [class_ "output", id_ "pointfree"] "")
            dt_ "Pointful"
            dd_ (div_ [class_ "output", id_ "pointful"] ""))
    p_
        (do "This page converts Haskell expressions between the pointfree and "
            "pointful styles. It is a web front end to the "
            pointfree_link
            " and "
            pointful_link
            " libraries. Read more about the pointfree style on "
            the_haskell_wiki_link
            ".")
    p_
        (do "Powered by "
            blunt_link
            " version "
            Version.versionString
            ". Created by "
            taylor_fausak_link
            ".")
    script_ [] Script.script

pointfree_link :: Html ()
pointfree_link =
    a_ [href_ "https://hackage.haskell.org/package/pointfree"] "pointfree"

pointful_link :: Html ()
pointful_link =
    a_ [href_ "https://hackage.haskell.org/package/pointful"] "pointful"

the_haskell_wiki_link :: Html ()
the_haskell_wiki_link =
    a_ [href_ "https://wiki.haskell.org/Pointfree"] "the Haskell wiki"

blunt_link :: Html ()
blunt_link = a_ [href_ "https://github.com/tfausak/blunt"] "Blunt"

taylor_fausak_link :: Html ()
taylor_fausak_link = a_ [href_ "http://taylor.fausak.me"] "Taylor Fausak"
