module Actions.Derivation
    ( action
    ) where

import           Algorithm.Regex.Derivation ( wordDerive )
import           Data.Regex                 ( Regex )
import           Data.Regex.Formats         ( toMinimallyQuotedText )
import           Language.RegexParser       ( parseRegex )

import           Data.Bifunctor             ( first )
import           Data.ByteString
import qualified Data.Text                  as T
import           Data.Text.Encoding         ( decodeUtf8' )
import           Snap.Core

data DerivationParameter
    = Regex
    | Word
    deriving (Show)

data DerivationError
    = ParameterNotFound DerivationParameter
    | Utf8DecodingError DerivationParameter
    | RegularExpressionParseError String

action :: Snap ()
action =
    method POST $ do
        setTimeout 10
        result <- derive <$> getParam "regex" <*> getParam "word"
        modifyResponse $ setContentType "text/plain; charset=utf-8"
        case result of
            Right derivedRegex ->
                writeText (toMinimallyQuotedText derivedRegex)
            Left e -> do
                modifyResponse $ setResponseCode 400
                case e of
                    ParameterNotFound parameter -> do
                        writeText . T.pack . show $ parameter
                        writeBS " was not specified"
                    Utf8DecodingError parameter -> do
                        writeText . T.pack . show $ parameter
                        writeBS " was not specified as a valid UTF-8 string"
                    RegularExpressionParseError parseError -> do
                        writeBS "Parse error in regular expression:\n"
                        writeText . T.pack . show $ parseError

derive :: Maybe ByteString -> Maybe ByteString -> Either DerivationError (Regex Char)
derive Nothing _ = Left (ParameterNotFound Regex)
derive _ Nothing = Left (ParameterNotFound Word)
derive (Just utf8InputRegexString) (Just utf8Word) = do
    inputRegexString <- first (const (Utf8DecodingError Regex)) $
        decodeUtf8' utf8InputRegexString
    word <- first (const (Utf8DecodingError Word)) $
        decodeUtf8' utf8Word
    case parseRegex "<regex>" inputRegexString of
        Left parseError -> Left (RegularExpressionParseError parseError)
        Right regex     -> return $ wordDerive (T.unpack word) regex
