{-# LANGUAGE FlexibleInstances #-}

module Actions.RegexToDfaConversion
    ( action
    ) where

import           Data.Dfa.Regex       ( RegexDfa, fromRegex, initialRegex )
import           Data.FiniteAutomaton
import           Data.Regex           ( Regex, matchesEmptyWord )
import           Data.Regex.Formats   ( MinimallyQuotedRegex (..) )
import           Language.RegexParser ( parseRegex )

import           Control.Monad        ( when )
import           Data.Aeson           ( ToJSON (toJSON), ToJSONKey (..),
                                        Value (String), object, (.=) )
import           Data.Aeson.Text      ( encodeToLazyText )
import           Data.Aeson.Types     ( Pair )
import           Data.ByteString.UTF8 as UTF8
import           Data.Maybe           ( isJust )
import           Data.Monoid          ( (<>) )
import qualified Data.Set             ( toList )
import qualified Data.Text            as TS
import           Snap.Core

action :: Snap ()
action =
    method POST $ do
        setTimeout 10
        modifyResponse $ setContentType "application/json; charset=utf-8"
        mRegexString <- getParam "regex"
        case mRegexString of
            Nothing -> badRequestError "No regex given"
            Just utf8RegexString -> do
                let regexString = UTF8.toString utf8RegexString
                when (UTF8.replacement_char `elem` regexString) $
                    badRequestError "UTF-8 decoding error"
                case parseRegex "<param>" regexString of
                    Left parseError ->
                        badRequestError . UTF8.fromString $ "Regex parse error:\n" <> parseError
                    Right regex -> do
                        withoutSkeleton <- (isJust . getHeader "X-Embeddable") <$> getRequest
                        writeLazyText . encodeToLazyText . JsonRegexDfa . fromRegex $ regex

badRequestError :: ByteString -> Snap ()
badRequestError description = do
    modifyResponse $ setResponseCode 400
    modifyResponse $ setContentType "text/plain; charset=utf-8"
    writeBS description
    finishWith =<< getResponse

data JsonRegexDfa =
    JsonRegexDfa (RegexDfa Char)

instance ToJSON (Regex Char) where
    toJSON = String . TS.pack . show . MinimallyQuotedRegex

instance ToJSON JsonRegexDfa where
    toJSON (JsonRegexDfa regexDfa) =
        object
            [ "alphabet" .= faInputs regexDfa
            , "initialRegex" .= initialRegex regexDfa
            , "regexes" .= (map regexData . Data.Set.toList . faStates $ regexDfa)
            , "transitionTable" .= object (transitionTable regexDfa)
            ]
      where
        transitionTable :: RegexDfa Char -> [Pair]
        transitionTable =
            map (\r -> toText (MinimallyQuotedRegex r) .= faTransitions regexDfa r) .
            Data.Set.toList . faStates

        regexData :: Regex Char -> Value
        regexData regex = object [
                "regex" .= (toText . MinimallyQuotedRegex) regex
              , "matchesEmptyWord" .= matchesEmptyWord regex
            ]

toText :: Show a => a -> TS.Text
toText = TS.pack . show
