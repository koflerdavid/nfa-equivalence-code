{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import RegexDerivation
import RegexEquivalence
import RegexFullDerivation
import Types

import Control.Monad.Trans.Except ( runExceptT )
import Options.Generic
import System.Exit
import System.IO

data Action = RegexFullDerivation { outputFormat    :: OutputFormat
                                  , withoutSkeleton :: Bool
                                  }
            | RegexDerivation String
            | RegexEquivalence
    deriving (Generic, Show)

instance ParseRecord Action

main :: IO ()
main = do
    command <- getRecord "regex derivation"
    case command of
        RegexFullDerivation format noSkeleton -> do
            result <- runExceptT (parseAndDeriveRegexToDfa format noSkeleton)
            case result of
                Left message -> printErrorAndExit message
                Right _      -> exitSuccess

        RegexDerivation word -> do
            result <- runExceptT (parseAndDeriveRegexByWord word)
            case result of
                Left message -> printErrorAndExit message
                Right _      -> exitSuccess

        RegexEquivalence -> do
            result <- runExceptT checkRegexEquivalence
            case result of
                Left message -> printErrorAndExit message
                Right True   -> exitSuccess
                Right False  -> exitWith (ExitFailure 1)

printErrorAndExit :: String -> IO a
printErrorAndExit msg = hPutStrLn stderr msg >> exitWith (ExitFailure 2)
