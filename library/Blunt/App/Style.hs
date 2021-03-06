{-# LANGUAGE OverloadedStrings #-}

module Blunt.App.Style where

import Clay
import Data.Monoid ((<>))
import qualified Data.Text.Lazy as Text

style :: Text.Text
style = renderWith compact [] css

css :: Css
css = do
    html <> body ?
        do backgroundColor "#f5f5f5"
           color "#151515"
           fontFamily [] [sansSerif]
           lineHeight (em 1.5)
           sym margin nil
           sym padding nil
    body ?
        do boxSizing borderBox
           sym2 margin nil auto
           maxWidth (em 40)
           padding nil (em 1.5) (em 1.5) (em 1.5)
    h1 ?
        do color "#90a959"
           fontSize (em 2)
           fontWeight bold
           lineHeight (em 3)
           sym margin nil
           textAlign (alignSide sideCenter)
    dl ? do sym margin nil
    dt ? do marginTop (em 1.5)
    dd ? do sym margin nil
    input <> ".output" ?
        do border solid (px 1) "#e0e0e0"
           boxSizing borderBox
           fontFamily [] [monospace]
           fontSize (em 1)
           width (pct 100)
    input ?
        do height (em 3)
           lineHeight (em 3)
           sym2 padding nil (em 0.75)
    ".output" ?
        do sym padding (em 0.75)
           whiteSpace preWrap
    p ? do margin (em 1.5) nil nil nil
