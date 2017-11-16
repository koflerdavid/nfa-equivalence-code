{-# LANGUAGE OverloadedStrings #-}

module Actions.RegexToDfaConversion  ( action ) where

import           Data.Dfa.Format.Html (asHtml)
import           Language.RegexParser (parseRegex)

import           Data.ByteString.UTF8 as UTF8
import           Data.Maybe           (isJust)
import           Snap.Core

action :: Snap ()
action =
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
            withoutSkeleton <-
              fmap (isJust . getHeader "X-Embeddable") getRequest
            writeLazyText (asHtml withoutSkeleton regex)
