{-# LANGUAGE OverloadedStrings #-}

module Blunt.Application where

import Blunt.Actions (convertAction, indexAction, notFoundAction)

import Network.Wai (Application, Request, Response, pathInfo, requestMethod)

application :: Application
application request respondWith = do
    let action = route request
    response <- action request
    respondWith response

route :: Request -> (Request -> IO Response)
route request = case (requestMethod request, pathInfo request) of
    ("GET", []) -> indexAction
    ("GET", ["convert"]) -> convertAction
    _ -> notFoundAction
