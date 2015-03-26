{-# LANGUAGE OverloadedStrings #-}

module Blunt.Router where

import Blunt.Actions (convertAction, indexAction, notFoundAction)
import Network.Wai (Request, Response, pathInfo, requestMethod)

route :: Request -> (Request -> IO Response)
route request = case (requestMethod request, pathInfo request) of
    ("GET", []) -> indexAction
    ("GET", ["convert"]) -> convertAction
    _ -> notFoundAction
