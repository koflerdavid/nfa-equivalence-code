{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Trans.Except ( runExceptT )
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
            result <- runExceptT (checkNfaEquivalence filename)
            case result of
                Left message -> printErrorAndExit message
                Right results ->
                    if and results
                        then exitSuccess
                        else exitWith (ExitFailure 1)
        DfaEquivalence filename -> do
            result <- runExceptT (checkDfaEquivalence filename)
            case result of
                Left message -> printErrorAndExit message
                Right results ->
                    if and results
                        then exitSuccess
                        else exitWith (ExitFailure 1)

printErrorAndExit :: String -> IO a
printErrorAndExit msg = hPutStrLn stderr msg >> exitWith (ExitFailure 2)
