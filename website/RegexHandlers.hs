{-# LANGUAGE OverloadedStrings #-}

module RegexHandlers where

import Algorithm.Regex.Derivation
import Language.RegexParser

import Data.ByteString.Char8 (unpack)
import Data.String (fromString)
import Snap.Core

derivationHandler :: Snap ()
derivationHandler =
  methods [POST] $ do
    setTimeout 10
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
