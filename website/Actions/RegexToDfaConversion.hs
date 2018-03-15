{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Actions.RegexToDfaConversion
    ( action
    ) where

import           Data.Dfa.Format.Hknt ( NameGenerator, toHknt )
import           Data.Dfa.Regex       ( RegexDfa, fromRegex, initialRegex )
import           Data.FiniteAutomaton
import           Data.Regex           ( Regex, matchesEmptyWord )
import           Data.Regex.Formats   ( MinimallyQuotedRegex (..) )
import           Language.RegexParser ( parseRegex )

import           Data.Aeson           ( ToJSON (toJSON), Value (String), object,
                                        (.=) )
import           Data.Aeson.Text      ( encodeToLazyText )
import           Data.Aeson.Types     ( Pair )
import           Data.Bifunctor       ( first )
import           Data.Monoid          ( (<>) )
import qualified Data.Set             ( toList )
import qualified Data.Text            as T
import           Data.Text.Encoding   ( decodeUtf8' )
import           Snap.Core

action :: Snap ()
action =
    method POST $ do
        setTimeout 10
        modifyResponse $ setContentType "application/json; charset=utf-8"
        mRegexString <- getParam "regex"
        either badRequestError (writeLazyText . encodeToLazyText . JsonRegexDfa) $ do
            utf8RegexString <- maybe (Left "No regex given") Right mRegexString
            regexString <- first (const "UTF-8 decoding error") $ decodeUtf8' utf8RegexString
            regex <-
                first (\err -> "Regex parse error:\n" <> T.pack err) $
                parseRegex "<param>" regexString
            return $! fromRegex regex

badRequestError :: T.Text -> Snap ()
badRequestError description = do
    modifyResponse $ setResponseCode 400
    modifyResponse $ setContentType "text/plain; charset=utf-8"
    writeText description
    finishWith =<< getResponse

newtype JsonRegexDfa = JsonRegexDfa (RegexDfa Char)

instance ToJSON (Regex Char) where
    toJSON = String . toText . MinimallyQuotedRegex

instance ToJSON JsonRegexDfa where
    toJSON (JsonRegexDfa regexDfa) =
        object
            [ "alphabet" .= faInputs regexDfa
            , "initialRegex" .= initialRegex regexDfa
            , "regexes" .= (map regexData . Data.Set.toList . faStates $ regexDfa)
            , "transitionTable" .= object (transitionTable regexDfa)
            , "hkntRepresentation" .= toHknt regexDfa regexNameGenerator
            ]
      where
        transitionTable :: RegexDfa Char -> [Pair]
        transitionTable =
            map (\r -> toText (MinimallyQuotedRegex r) .= faTransitions regexDfa r)
            . Data.Set.toList
            . faStates

        regexData :: Regex Char -> Value
        regexData regex =
            object
                [ "regex" .= (toText . MinimallyQuotedRegex) regex
                , "matchesEmptyWord" .= matchesEmptyWord regex
                ]

regexNameGenerator :: NameGenerator Int (Regex Char) T.Text
regexNameGenerator = (1, nextName)
  where
    nextName _regex state = (succ state, "r" <> toText state)

toText :: Show a => a -> T.Text
toText = T.pack . show
