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
import           Data.ByteString.UTF8        as UTF8
import           Data.Monoid                 ( (<>) )
import qualified Data.Text                   as TS
import qualified Data.Text.Lazy              as T
import           Snap.Core

data Parameter
    = Regex1
    | Regex2

asText :: Parameter -> T.Text
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

newtype JsonRegex =
    JsonRegex (Regex Char)

type Trace = [TraceElement]

data EquivalenceResult
    = Equivalent Trace
    | NotEquivalent [String]
                    Trace

instance ToJSON EquivalenceResult where
    toJSON (Equivalent trace) = object ["equivalent" .= True, "trace" .= trace]
    toJSON (NotEquivalent witnesses trace) =
        object ["equivalent" .= False, "witnesses" .= witnesses, "trace" .= trace]

instance ToJSON JsonRegex where
    toJSON (JsonRegex regex) = String . TS.pack . show $ MinimallyQuotedRegex regex

instance ToJSON TraceElement where
    toJSON traceElement =
        let (input, regex1, regex2) = traceConstraint traceElement
        in object
               [ "checked" .= traceConsidered traceElement
               , "constraint" .=
                 object
                     ["input" .= input, "regex1" .= JsonRegex regex1, "regex2" .= JsonRegex regex2]
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
                        T.pack syntaxError
            Right result -> do
                modifyResponse $ setContentType "application/json"
                writeLazyText . encodeToLazyText $ result

equivalent :: Maybe ByteString -> Maybe ByteString -> Either EquivalenceError EquivalenceResult
equivalent Nothing _ = Left (RegexMissing Regex1)
equivalent _ Nothing = Left (RegexMissing Regex2)
equivalent (Just utf8RegexString1) (Just utf8RegexString2) = do
    regexString1 <-
        maybe (Left $ Utf8DecodeError Regex1) Right (fromUtf8 utf8RegexString1)
    regexString2 <-
        maybe (Left $ Utf8DecodeError Regex2) Right (fromUtf8 utf8RegexString2)
    regex1 <- first (RegexParseError Regex1) $ parseRegex "regex1" regexString1
    regex2 <- first (RegexParseError Regex2) $ parseRegex "regex2" regexString2
    let (witnesses, trace) = getDifferences regex1 regex2
    if null witnesses
        then return $ Equivalent (map convertTraceElement trace)
        else return $
             NotEquivalent
                 (map (\(witness, _, _) -> witness) witnesses)
                 (map convertTraceElement trace)

fromUtf8 :: ByteString -> Maybe String
fromUtf8 bs = do
    let str = UTF8.toString bs
    if UTF8.replacement_char `elem` str
        then Nothing
        else Just str

convertTraceElement :: (Bool, (String, Regex Char, Regex Char)) -> TraceElement
convertTraceElement (skipped, witness) =
    TraceElement {traceConsidered = not skipped, traceConstraint = witness}
