module Blunt.Script where

script :: String
script = unlines js

js :: [String]
js =
    [ "'use strict';"
    , ""
    , "(function () {"
    , "  var input = document.getElementById('input');"
    , "  var pointfree = document.getElementById('pointfree');"
    , "  var pointful = document.getElementById('pointful');"
    , ""
    , "  var updateHash = function () {"
    , "    window.location.replace('#input=' + input.value);"
    , "  };"
    , ""
    , "  var updateOutput = function () {"
    , "    var request = new XMLHttpRequest();"
    , ""
    , "    request.onreadystatechange = function () {"
    , "      if (request.readyState === 4 && request.status === 200) {"
    , "        var response = JSON.parse(request.response);"
    , ""
    , "        pointfree.textContent = response.pointfree.join('\\n');"
    , "        pointful.textContent = response.pointful;"
    , "      }"
    , "    };"
    , "    request.open('GET', '/convert?input=' + encodeURIComponent(input.value));"
    , "    request.send();"
    , "  };"
    , ""
    , "  input.oninput = function (_event) {"
    , "    updateHash();"
    , "    updateOutput();"
    , "  };"
    , ""
    , "  if (window.location.hash.indexOf('#input=') === 0) {"
    , "    input.value = window.location.hash.substring(7);"
    , "    input.oninput();"
    , "  }"
    , "}());"
    ]
