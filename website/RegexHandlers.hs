{-# LANGUAGE OverloadedStrings #-}

module RegexHandlers where

import Algorithm.Regex.Derivation
import Algorithm.Regex.DfaConversion   ( deriveRegexToDfa )
import Data.Dfa.Format.Html            ( asHtml )
import Data.Regex
import Language.RegexParser

import Data.ByteString.Char8           ( unpack )
import Data.Map
import Data.Maybe                      ( isNothing )
import Data.String                     ( fromString )
import Snap.Core

type RegexDfaTransitions = Map (Regex Char) (Map Char (Regex Char))

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

regexToDfaConversionHandler :: Snap ()
regexToDfaConversionHandler =
  methods [POST] $ do
    setTimeout 10
    modifyResponse $ setContentType "text/html; charset=utf-8"
    mRegexString <- getParam "regex"
    case mRegexString of
      Nothing -> writeBS "No regex given"
      Just regexString ->
        case parseRegex "<param>" (unpack regexString) of
          Left _parseError -> writeBS "Regex parse error"
          Right regex -> do
            let transitions = deriveRegexToDfa regex
            withHeader <- fmap (isNothing . getHeader "X-Embeddable") getRequest
            writeLazyText (asHtml withHeader regex transitions)
