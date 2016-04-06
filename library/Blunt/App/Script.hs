{-# LANGUAGE QuasiQuotes #-}

module Blunt.App.Script where

import Data.Function ((&))
import qualified Data.Text.Lazy as Text
import Language.Javascript.JMacro
import qualified Text.PrettyPrint.Leijen.Text as PrettyPrint

script :: Text.Text
script = js & renderJs & PrettyPrint.renderOneLine & PrettyPrint.displayT

js :: JStat
js =
    [jmacro|
\ {
    var input = document.getElementById("input");
    var pointfree = document.getElementById("pointfree");
    var pointful = document.getElementById("pointful");

    var socket = new WebSocket(window.location.origin.replace("http", "ws"));

    socket.onopen = \ {
        input.oninput = \ {
            window.location.replace(
                "#input=" + encodeURIComponent(input.value));
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
        input.value = decodeURIComponent(window.location.hash.substring(7));
    }
}();
|]
