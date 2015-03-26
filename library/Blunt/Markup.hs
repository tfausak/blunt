module Blunt.Markup where

import Blunt.Script (script)
import Blunt.Style (style)

markup :: String
markup = unlines html

html :: [String]
html =
    [ "<!doctype html>"
    , ""
    , "<html>"
    , "  <head>"
    , "    <meta charset='utf-8'>"
    , "    <meta name='viewport' content='initial-scale = 1, maximum-scale = 1, minimum-scale = 1, width = device-width'>"
    , ""
    , "    <title>Blunt</title>"
    , ""
    , "    <style>"
    , style
    , "    </style>"
    , "  </head>"
    , ""
    , "  <body>"
    , "    <h1>Blunt</h1>"
    , ""
    , "    <dl>"
    , "      <dt>Input</dt>"
    , "      <dd>"
    , "        <input id='input' placeholder='sum xs = foldr (+) 0 xs' autocapitalize='none' autocomplete='off' autocorrect='off' autofocus spellcheck='false'>"
    , "      </dd>"
    , ""
    , "      <dt>Pointfree</dt>"
    , "      <dd>"
    , "        <div id='pointfree'></div>"
    , "      </dd>"
    , ""
    , "      <dt>Pointful</dt>"
    , "      <dd>"
    , "        <div id='pointful'></div>"
    , "      </dd>"
    , "    </dl>"
    , ""
    , "    <p>"
    , "      <a href='https://github.com/tfausak/blunt'>"
    , "        https://github.com/tfausak/blunt"
    , "      </a>"
    , "    </p>"
    , ""
    , "    <script>"
    , script
    , "    </script>"
    , "  </body>"
    , "</html>"
    ]
