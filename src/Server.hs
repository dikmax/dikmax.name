{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           FileServe
import           Snap.Core
import           Snap.Http.Server

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site = path "rss" (redirect "feed.rss") <|>
    path "rss/" (redirect "feed.rss") <|>
    dir "media" (serveDirectory "/home/dikmax/Dropbox/dikmax.name") <|>

#ifdef DEVELOPMENT
    dir "dart" (serveDirectory "dart") <|>
    dir "js" (serveDirectory "js") <|>
#endif

    serveDirectory "_site" <|>
    notFoundHandler


notFoundHandler :: Snap ()
notFoundHandler = do
  modifyResponse $ setResponseCode 404
  sendFile "_site/404/index.html"
