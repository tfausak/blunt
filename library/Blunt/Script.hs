{-# LANGUAGE QuasiQuotes #-}

module Blunt.Script where

import Language.Javascript.JMacro

script :: String
script = show (renderJs js)

js :: JStat
js = [jmacro|
    var input = document.getElementById("input");
    var pointfree = document.getElementById("pointfree");
    var pointful = document.getElementById("pointful");

    var updateHash = \ { window.location.replace("#input=" + input.value); };

    var updateOutput = \ {
        var request = new XMLHttpRequest;

        request.onreadystatechange = \ {
            if (request.readyState !== 4 || request.status !== 200) { return; }

            var response = JSON.parse request.response;
            pointfree.textContent = response.pointfree.join("\n");
            pointful.textContent = response.pointful;
        };

        request.open("GET", "/convert?input=" + encodeURIComponent(input.value));
        request.send();
    };

    input.oninput = \ {
        updateHash();
        updateOutput();
    };
    
    if (window.location.hash.indexOf("#input=") === 0) {
        input.value = window.location.hash.substring(7);
        input.oninput();
    }
|]
