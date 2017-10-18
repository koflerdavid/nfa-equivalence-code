{-# LANGUAGE OverloadedStrings #-}

module Main where

import RegexHandlers

import Control.Applicative
import Snap.Core
import Snap.Http.Server
import Snap.Util.FileServe

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site =
  ifTop (sendFile "website/static/index.html") <|>
  route
    [ ("regex/derivation", derivationHandler)
    ] <|>
  dir "static" (serveDirectory "website/static")
