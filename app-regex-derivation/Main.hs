{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.Trans.Except ( runExceptT )
import           Options.Generic
import           System.Exit
import           System.IO

import           RegexFullDerivation

data Action = RegexFullDerivation
    deriving (Generic, Show)

instance ParseRecord Action

main :: IO ()
main = do
    command <- getRecord "regex derivation"
    case command of
        RegexFullDerivation -> do
            result <- runExceptT parseAndDeriveRegexToDfa
            case result of
                Left message -> printErrorAndExit message
                Right results -> exitSuccess

printErrorAndExit :: String -> IO a
printErrorAndExit msg = hPutStrLn stderr msg >> exitWith (ExitFailure 2)
