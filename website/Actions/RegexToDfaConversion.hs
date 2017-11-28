module Actions.RegexToDfaConversion
    ( action
    ) where

import Data.Dfa.Format.Html ( asHtml )
import Language.RegexParser ( parseRegex )

import Control.Monad        ( when )
import Data.ByteString.UTF8 as UTF8
import Data.Maybe           ( isJust )
import Data.Monoid          ( (<>) )
import Snap.Core

action :: Snap ()
action =
    method POST $ do
        setTimeout 10
        modifyResponse $ setContentType "text/html; charset=utf-8"
        mRegexString <- getParam "regex"
        case mRegexString of
            Nothing -> badRequestError "No regex given"
            Just utf8RegexString -> do
                let regexString = UTF8.toString utf8RegexString
                when (UTF8.replacement_char `elem` regexString) $
                    badRequestError "UTF-8 decoding error"
                case parseRegex "<param>" regexString of
                    Left parseError ->
                        badRequestError . UTF8.fromString $
                        "Regex parse error:\n" <> parseError
                    Right regex -> do
                        withoutSkeleton <-
                            (isJust . getHeader "X-Embeddable") <$> getRequest
                        writeLazyText (asHtml withoutSkeleton regex)

badRequestError :: ByteString -> Snap ()
badRequestError description = do
    modifyResponse $ setResponseCode 400
    modifyResponse $ setContentType "text/plain; charset=utf-8"
    writeBS description
    finishWith =<< getResponse
