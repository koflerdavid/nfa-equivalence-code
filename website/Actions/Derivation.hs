module Actions.Derivation
    ( action
    ) where

import Algorithm.Regex.Derivation ( wordDerive )
import Data.Regex                 ( Regex )
import Language.RegexParser       ( parseRegex )

import Control.Monad              ( when )
import Data.ByteString.UTF8       as UTF8
import Snap.Core

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
            Right derivedRegex -> writeBS $ UTF8.fromString (show derivedRegex)
            Left e -> do
                modifyResponse $ setResponseCode 400
                case e of
                    ParameterNotFound parameter -> do
                        writeBS $ UTF8.fromString . show $ parameter
                        writeBS " was not specified"
                    Utf8DecodingError parameter -> do
                        writeBS $ UTF8.fromString . show $ parameter
                        writeBS " was not specified as a valid UTF-8 string"
                    RegularExpressionParseError parseError -> do
                        writeBS "Parse error in regular expression:\n"
                        writeBS (UTF8.fromString parseError)

derive ::
       Maybe ByteString
    -> Maybe ByteString
    -> Either DerivationError (Regex Char)
derive Nothing _ = Left (ParameterNotFound Regex)
derive _ Nothing = Left (ParameterNotFound Word)
derive (Just utf8InputRegexString) (Just utf8Word) = do
    let inputRegexString = UTF8.toString utf8InputRegexString
        word = UTF8.toString utf8Word
    when (UTF8.replacement_char `elem` inputRegexString) $
        Left (Utf8DecodingError Regex)
    when (UTF8.replacement_char `elem` word) $ Left (Utf8DecodingError Word)
    case parseRegex "<regex>" inputRegexString of
        Left parseError -> Left (RegularExpressionParseError parseError)
        Right regex     -> return $ wordDerive word regex
