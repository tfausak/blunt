{-# LANGUAGE OverloadedStrings #-}

module Blunt.Markup where

import Flow
import Lucid

import Blunt.Script (script)
import Blunt.Style (style)
import Data.ByteString.Lazy (ByteString)

markup :: ByteString
markup = renderBS html

html :: Html ()
html = doctypehtml_ <| do
    head_ <| do
        meta_
            [ name_ "viewport"
            , content_ "initial-scale = 1, maximum-scale = 1, minimum-scale = 1, width = device-width"
            ]

        title_ "Blunt"

        style_ [] style

    body_ <| do
        h1_ "Blunt"

        dl_ <| do
            dt_ "Input"
            dd_ <| do
                input_
                    [ id_ "input"
                    , placeholder_ "sum xs = foldr (+) 0 xs"
                    , autocomplete_ "off"
                    , autofocus_
                    , spellcheck_ "off"
                    , term "autocapitalize" "none"
                    , term "autocorrect" "off"
                    ]

            dt_ "Pointfree"
            dd_ (div_ [id_ "pointfree"] "")

            dt_ "Pointful"
            dd_ (div_ [id_ "pointful"] "")

        p_ <| do
            "Blunt converts Haskell expressions between the pointfree and "
            "pointful styles. It is a web front end to the "
            a_ [href_ "http://hackage.haskell.org/package/pointfree"]
                "pointfree"
            " and "
            a_ [href_ "http://hackage.haskell.org/package/pointful"]
                "pointful"
            " libraries."

        p_ <| do
            a_ [href_ "https://github.com/tfausak/blunt"] <| do
                "github.com/tfausak/blunt"

        script_ [] script
