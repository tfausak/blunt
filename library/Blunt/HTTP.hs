{-# LANGUAGE OverloadedStrings #-}

module Blunt.HTTP where

import Blunt.Markup (markup)
import Network.HTTP.Types (notFound404, ok200)
import Network.Wai (Application, Middleware, pathInfo, requestMethod,
    responseLBS)
import Network.Wai.Middleware.Gzip (def, gzip)
import Network.Wai.Middleware.RequestLogger (logStdout)

httpApplication :: Application
httpApplication = middleware application where

    middleware :: Middleware
    middleware = gzip def . logStdout

    application :: Application
    application request respond = respond $
        case (requestMethod request, pathInfo request) of
            ("GET", []) ->
                responseLBS ok200 [("Content-Type", "text/html")] markup
            _ ->
                responseLBS notFound404 [] ""
