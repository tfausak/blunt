module Blunt.Style where

style :: String
style = unlines
    [ "html, body {"
    , "  background: #f5f5f5;"
    , "  color: #151515;"
    , "  font: 100%/1.5em sans-serif;"
    , "  margin: 0;"
    , "  padding: 0;"
    , "}"
    , ""
    , "body {"
    , "  box-sizing: border-box;"
    , "  margin: 0 auto;"
    , "  max-width: 40em;"
    , "  padding: 0 1.5em;"
    , "}"
    , ""
    , "h1 {"
    , "  color: #90a959;"
    , "  font-size: 2em;"
    , "  font-weight: bold;"
    , "  line-height: 3em;"
    , "  margin: 0;"
    , "  text-align: center;"
    , "}"
    , ""
    , "dl {"
    , "  margin: 0;"
    , "}"
    , ""
    , "dt {"
    , "  margin-top: 1.5em;"
    , "}"
    , ""
    , "dd {"
    , "  margin: 0;"
    , "}"
    , ""
    , "input, div {"
    , "  border: thin solid #e0e0e0;"
    , "  box-sizing: border-box;"
    , "  font-family: monospace;"
    , "  font-size: 1em;"
    , "  width: 100%;"
    , "}"
    , ""
    , "input {"
    , "  height: 3em;"
    , "  line-height: 3em;"
    , "  padding: 0 0.75em;"
    , "}"
    , ""
    , "div {"
    , "  padding: 0.75em;"
    , "  white-space: pre-wrap;"
    , "}"
    , ""
    , "p {"
    , "  margin: 1.5em 0 0 0;"
    , "  text-align: center;"
    , "}"
    ]