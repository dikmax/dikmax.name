{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Snap.Core
import           FileServe
import           Snap.Http.Server

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site = serveDirectory "_site"

