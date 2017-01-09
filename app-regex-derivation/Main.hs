{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           RegexDerivation
import           RegexFullDerivation
import           Types

import           Control.Monad.Trans.Except ( runExceptT )
import           Options.Generic
import           System.Exit
import           System.IO

data Action = RegexFullDerivation { outputFormat    :: OutputFormat
                                  , withoutSkeleton :: Bool
                                  }
            | RegexDerivation String
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
                Right _ -> exitSuccess

        RegexDerivation word -> do
            result <- runExceptT (parseAndDeriveRegexByWord word)
            case result of
                Left message -> printErrorAndExit message
                Right _ -> exitSuccess

printErrorAndExit :: String -> IO a
printErrorAndExit msg = hPutStrLn stderr msg >> exitWith (ExitFailure 2)
