{-# LANGUAGE OverloadedStrings #-}

module Main where

import Algorithm.Regex.Derivation
import Language.RegexParser

import Control.Applicative
import Data.ByteString.Char8 (unpack)
import Data.String (fromString)
import Snap.Core
import Snap.Http.Server
import Snap.Util.FileServe

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site =
  ifTop (sendFile "website/static/index.html") <|>
  route [
      ("regex/derivation", derivationHandler)
    ] <|>
  dir "static" (serveDirectory ".")

echoHandler :: Snap ()
echoHandler = do
  param <- getParam "echoparam"
  maybe (writeBS "must specify echo/param in URL") writeBS param

derivationHandler :: Snap ()
derivationHandler = methods [POST] $ do
  setTimeout 30
  modifyResponse $ setContentType "text/plain; charset=utf-8"

  mInputRegexString <- getParam "regex"
  mWord <- getParam "word"
  case (mInputRegexString, mWord) of
    (Nothing, _) -> writeBS "No regex given"
    (_, Nothing) -> writeBS "No word given"
    (Just inputRegexString, Just word) ->
      case parseRegex "<param>" (unpack inputRegexString) of
        Left _parseError -> writeBS "parse error"
        Right regex -> do
          let regex' = wordDerive (unpack word) regex
          writeBS $ fromString (show regex')
