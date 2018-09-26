{-# LANGUAGE OverloadedStrings #-}
module Main  where

import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)

notFound :: Response
notFound = responseLBS status404 [("Content-Type", "text/plain")] "404 - Not Found"

indexJS :: Response
indexJS = responseFile status200 [("Content-Type", "application/javascript")] "web/index.js" Nothing

index :: Response
index = responseFile status200 [("Content-Type", "text/html")] "web/index.html" Nothing

screenCSS :: Response
screenCSS = responseFile status200 [("Content-Type", "text/css")] "web/index.css" Nothing

app :: Application
app request respond = respond $ case rawPathInfo request of
  "/" ->
    index
  "/index.js" ->
    indexJS
  "/index.css" ->
    screenCSS
  _ ->
    notFound

start :: Int -> IO ()
start port = run port app

main = start 8000
