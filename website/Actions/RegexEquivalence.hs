module Actions.RegexEquivalence
    ( action
    ) where

import           Algorithm.Regex.Equivalence ( getDifferences )
import           Language.RegexParser        ( parseRegex )

import           Data.Aeson                  ( ToJSON (toJSON), object, (.=) )
import           Data.Aeson.Text             ( encodeToLazyText )
import           Data.Bifunctor              ( first )
import           Data.ByteString.UTF8        as UTF8
import           Data.Monoid                 ( (<>) )
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

data EquivalenceResult
    = Equivalent
    | NotEquivalent [String]

instance ToJSON EquivalenceResult where
    toJSON Equivalent = object ["equivalent" .= True]
    toJSON (NotEquivalent witnesses) =
        object ["equivalent" .= False, "witnesses" .= witnesses]

action :: Snap ()
action =
    method POST $ do
        output <- equivalent <$> getParam "regex1" <*> getParam "regex2"
        case output of
            Left e -> do
                modifyResponse $ setResponseCode 400
                modifyResponse $ setContentType "text/plain; charset=utf-8"
                case e of
                    RegexMissing p ->
                        writeLazyText $
                        "The parameter " <> asText p <> " is missing"
                    Utf8DecodeError p ->
                        writeLazyText $
                        "The parameter " <> asText p <>
                        " contains invalid UTF8 codepoints"
                    RegexParseError p syntaxError ->
                        writeLazyText $
                        "The parameter " <> asText p <>
                        " contains a syntax error:\n" <>
                        T.pack syntaxError
            Right result -> do
                modifyResponse $ setContentType "application/json"
                writeLazyText . encodeToLazyText $ result

equivalent ::
       Maybe ByteString
    -> Maybe ByteString
    -> Either EquivalenceError EquivalenceResult
equivalent Nothing _ = Left (RegexMissing Regex1)
equivalent _ Nothing = Left (RegexMissing Regex2)
equivalent (Just utf8RegexString1) (Just utf8RegexString2) = do
    regexString1 <-
        maybe (Left $ Utf8DecodeError Regex1) Right (fromUtf8 utf8RegexString1)
    regexString2 <-
        maybe (Left $ Utf8DecodeError Regex2) Right (fromUtf8 utf8RegexString2)
    regex1 <- first (RegexParseError Regex1) $ parseRegex "regex1" regexString1
    regex2 <- first (RegexParseError Regex2) $ parseRegex "regex2" regexString2
    let witnesses = getDifferences regex1 regex2
    if null witnesses
        then return Equivalent
        else return . NotEquivalent . map (\(witness, _, _) -> witness) $
             witnesses

fromUtf8 :: ByteString -> Maybe String
fromUtf8 bs = do
    let str = UTF8.toString bs
    if UTF8.replacement_char `elem` str
        then Nothing
        else Just str
