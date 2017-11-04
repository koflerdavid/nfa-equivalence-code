{-# LANGUAGE OverloadedStrings #-}

module RegexHandlers where

import Algorithm.Regex.Derivation
import Algorithm.Regex.DfaConversion ( deriveRegexToDfa )
import Data.Dfa.Format.Html          ( asHtml )
import Data.Regex                    ( Regex )
import Language.RegexParser

import Control.Monad                 ( when )
import Data.ByteString.UTF8          as UTF8
import Data.Maybe                    ( isJust )
import Snap.Core

data DerivationParameter = Regex | Word
  deriving (Show)

data DerivationError
  = ParameterNotFound DerivationParameter
  | Utf8DecodingError DerivationParameter
  | RegularExpressionParseError

derivationHandler :: Snap ()
derivationHandler =
  method POST $ do
    setTimeout 10
    result <- derivationHandler' <$> getParam "regex" <*> getParam "word"
    case result of
      Right regex' -> do
        modifyResponse $ setContentType "text/plain; charset=utf-8"
        writeBS $ UTF8.fromString (show regex')
      Left e -> do
        modifyResponse $ setResponseCode 400
        case e of
          ParameterNotFound parameter -> do
            writeBS $ UTF8.fromString . show $ parameter
            writeBS " was not specified"
          Utf8DecodingError parameter -> do
            writeBS $ UTF8.fromString . show $ parameter
            writeBS " was not specified as a valid UTF-8 string"
          RegularExpressionParseError ->
            writeBS "Parse error in regular expression"

derivationHandler' :: Maybe ByteString -> Maybe ByteString -> Either DerivationError (Regex Char)
derivationHandler' Nothing _ = Left (ParameterNotFound Regex)
derivationHandler' _ Nothing = Left (ParameterNotFound Word)
derivationHandler' (Just utf8InputRegexString) (Just utf8Word) = do
  let inputRegexString = UTF8.toString utf8InputRegexString
      word = UTF8.toString utf8Word
  when (UTF8.replacement_char `elem` inputRegexString) $
    Left (Utf8DecodingError Regex)
  when (UTF8.replacement_char `elem` word) $
    Left (Utf8DecodingError Word)
  case parseRegex "<param>" inputRegexString of
    Left _parseError -> Left RegularExpressionParseError
    Right regex -> return $ wordDerive word regex

regexToDfaConversionHandler :: Snap ()
regexToDfaConversionHandler =
  method POST $ do
    setTimeout 10
    modifyResponse $ setContentType "text/html; charset=utf-8"
    mRegexString <- getParam "regex"
    case mRegexString of
      Nothing -> writeBS "No regex given"
      Just regexString ->
        case parseRegex "<param>" (UTF8.toString regexString) of
          Left _parseError -> writeBS "Regex parse error"
          Right regex -> do
            let transitions = deriveRegexToDfa regex
            withoutSkeleton <-
              fmap (isJust . getHeader "X-Embeddable") getRequest
            writeLazyText (asHtml withoutSkeleton regex transitions)
