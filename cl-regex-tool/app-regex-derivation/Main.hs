{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import RegexDerivation
import RegexEquivalence
import RegexFullDerivation
import Types

import Control.Exception.Safe (Exception(displayException), try)
import Options.Generic
import System.Exit
import System.IO

data Action
    = RegexFullDerivation { outputFormat    :: OutputFormat
                          , withoutSkeleton :: Bool }
    | RegexDerivation String
    | RegexEquivalence
    deriving (Generic, Show)

instance ParseRecord Action

main :: IO ()
main = do
    command <- getRecord "regex derivation"
    case command of
        RegexFullDerivation format noSkeleton -> do
            result <- try (parseAndDeriveRegexToDfa format noSkeleton)
            case result :: Either RegexParseException () of
                Left message -> printErrorAndExit message
                Right _      -> exitSuccess
        RegexDerivation word -> do
            result <- try (parseAndDeriveRegexByWord word)
            case result :: Either RegexParseException () of
                Left message -> printErrorAndExit message
                Right _      -> exitSuccess
        RegexEquivalence -> do
            result <- try checkRegexEquivalence
            case result :: Either RegexParseException Bool of
                Left message -> printErrorAndExit message
                Right True   -> exitSuccess
                Right False  -> exitWith (ExitFailure 1)

printErrorAndExit :: Exception e => e -> IO ()
printErrorAndExit exception = hPutStrLn stderr (displayException exception) >> exitWith (ExitFailure 2)
