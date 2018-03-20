{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Actions.RegexEquivalence
    ( action
    ) where

import           Algorithm.Regex.Equivalence ( Witness, getDifferences )
import           Data.Regex                  ( Regex )
import           Data.Regex.Formats          ( MinimallyQuotedRegex (..) )
import           Language.RegexParser        ( parseRegex )

import           Data.Aeson                  ( ToJSON (toJSON), Value (String),
                                               object, (.=) )
import           Data.Aeson.Text             ( encodeToLazyText )
import           Data.Bifunctor              ( first )
import           Data.ByteString             ( ByteString )
import           Data.Monoid                 ( (<>) )
import qualified Data.Text                   as T
import           Data.Text.Encoding          ( decodeUtf8' )
import qualified Data.Text.Lazy              as TL
import           Snap.Core

data Parameter
    = Regex1
    | Regex2

asText :: Parameter -> TL.Text
asText Regex1 = "regex 1"
asText Regex2 = "regex 2"

data EquivalenceError
    = RegexMissing Parameter
    | Utf8DecodeError Parameter
    | RegexParseError Parameter
                      String

data TraceElement = TraceElement
    { traceConsidered :: Bool
    , traceConstraint :: Witness Char
    }

type Trace = [TraceElement]

data EquivalenceResult
    = Equivalent Trace
    | NotEquivalent [String]
                    Trace

instance ToJSON EquivalenceResult where
    toJSON (Equivalent trace) = object ["equivalent" .= True, "trace" .= trace]
    toJSON (NotEquivalent witnesses trace) =
        object ["equivalent" .= False, "witnesses" .= witnesses, "trace" .= trace]

instance ToJSON (MinimallyQuotedRegex Char) where
    toJSON = String . T.pack . show

instance ToJSON TraceElement where
    toJSON traceElement =
        let (input, regex1, regex2) = traceConstraint traceElement
        in object
               [ "checked" .= traceConsidered traceElement
               , "constraint" .=
                 object
                     [ "input" .= input
                     , "regex1" .= MinimallyQuotedRegex regex1
                     , "regex2" .= MinimallyQuotedRegex regex2
                     ]
               ]

action :: Snap ()
action =
    method POST $ do
        output <- equivalent <$> getParam "regex1" <*> getParam "regex2"
        case output of
            Left e -> do
                modifyResponse $ setResponseCode 400
                modifyResponse $ setContentType "text/plain; charset=utf-8"
                case e of
                    RegexMissing p -> writeLazyText $ "The parameter " <> asText p <> " is missing"
                    Utf8DecodeError p ->
                        writeLazyText $
                            "The parameter " <> asText p <> " contains invalid UTF8 codepoints"
                    RegexParseError p syntaxError ->
                        writeLazyText $
                            "The parameter " <> asText p <> " contains a syntax error:\n" <>
                            TL.pack syntaxError
            Right result -> do
                modifyResponse $ setContentType "application/json"
                writeLazyText . encodeToLazyText $ result

equivalent :: Maybe ByteString -> Maybe ByteString -> Either EquivalenceError EquivalenceResult
equivalent Nothing _ = Left (RegexMissing Regex1)
equivalent _ Nothing = Left (RegexMissing Regex2)
equivalent (Just utf8RegexString1) (Just utf8RegexString2) = do
    regexString1 <- first (const (Utf8DecodeError Regex1)) $ decodeUtf8' utf8RegexString1
    regexString2 <- first (const (Utf8DecodeError Regex2)) $ decodeUtf8' utf8RegexString2
    regex1 <- first (RegexParseError Regex1) $ parseRegex "regex1" regexString1
    regex2 <- first (RegexParseError Regex2) $ parseRegex "regex2" regexString2
    let (witnesses, trace) = getDifferences regex1 regex2
    if null witnesses
        then return $ Equivalent (map convertTraceElement trace)
        else return $
             NotEquivalent
                 (map (\(witness, _, _) -> witness) witnesses)
                 (map convertTraceElement trace)

convertTraceElement :: (Bool, (String, Regex Char, Regex Char)) -> TraceElement
convertTraceElement (skipped, witness) =
    TraceElement {traceConsidered = not skipped, traceConstraint = witness}
