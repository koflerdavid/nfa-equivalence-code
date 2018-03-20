{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Exception ( Exception(displayException), try )
import Options.Generic
import System.Exit
import System.IO

import DfaChecking
import NfaChecking

data Action
    = DfaEquivalence (Maybe String)
    | NfaEquivalence (Maybe String)
    deriving (Generic, Show)

instance ParseRecord Action

main :: IO ()
main = do
    command <- getRecord "automata equivalence"
    case command of
        NfaEquivalence filename -> do
            result :: Either NfaCheckingException [Bool] <- try (checkNfaEquivalence filename)
            case result of
                Left exception -> printErrorAndExit exception
                Right results ->
                    if and results
                        then exitSuccess
                        else exitWith (ExitFailure 1)
        DfaEquivalence filename -> do
            result :: Either DfaCheckingException [Bool] <- try (checkDfaEquivalence filename)
            case result of
                Left exception -> printErrorAndExit exception
                Right results -> do
                    if and results
                        then exitSuccess
                        else exitWith (ExitFailure 1)

printErrorAndExit :: Exception e => e -> IO ()
printErrorAndExit exception = hPutStrLn stderr (displayException exception) >> exitWith (ExitFailure 2)
