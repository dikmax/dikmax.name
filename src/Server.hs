{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Control.Monad.IO.Class
import           Data.ByteString
import qualified Data.ByteString.Char8 as BS
import           FileServe
import           Snap.Core
import           Snap.Http.Server
import           System.Process

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site = path "github-webhook" githubWebhook <|>
    path "rss" (redirect "feed.rss") <|>
    path "rss/" (redirect "feed.rss") <|>
    serveDirectory "_site" <|>
    notFoundHandler


githubWebhook :: Snap ()
githubWebhook = do
  request <- getRequest
  let ip = rqRemoteAddr request
  if ip == "127.0.0.1" || "204.232.175." `isPrefixOf` ip || "192.30.252." `isPrefixOf` ip
    then do
      output <- liftIO $ readProcess "./update.sh" [] ""
      writeBS $ BS.pack output
    else writeBS "Fail"

notFoundHandler :: Snap ()
notFoundHandler = do
  modifyResponse $ setResponseCode 404
  sendFile "_site/404/index.html"
