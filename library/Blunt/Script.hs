{-# LANGUAGE QuasiQuotes #-}

module Blunt.Script where

import Language.Javascript.JMacro

import Data.Text.Lazy (Text)
import Text.PrettyPrint.Leijen.Text (displayT, renderOneLine)

script :: Text
script = displayT (renderOneLine (renderJs js))

js :: JStat
js = [jmacro| \ {
    var input = document.getElementById("input");
    var pointfree = document.getElementById("pointfree");
    var pointful = document.getElementById("pointful");

    var socket = new WebSocket(window.location.origin.replace('http', 'ws'));

    socket.onopen = \ {
        input.oninput = \ {
            window.location.replace("#input=" + input.value);
            socket.send(input.value);
        };

        if (input.value) { input.oninput(); }
    };

    socket.onmessage = \ message {
        var response = JSON.parse(message.data);
        pointfree.textContent = response.pointfree.join("\n");
        pointful.textContent = response.pointful;
    };

    if (window.location.hash.indexOf("#input=") === 0) {
        input.value = window.location.hash.substring(7);
    }
}(); |]
