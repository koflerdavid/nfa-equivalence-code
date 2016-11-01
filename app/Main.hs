{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where


import Data.Dfa
import Data.Nfa
import Data.Regex
import Options.Generic
import System.Exit
import System.IO

import Algorithm.DfaEquivalence
import Algorithm.HkntToDfa
import Language.Automata.HkntParser

data Action
  = DfaEquivalence (Maybe String)
  | NfaEquivalence (Maybe String)
  deriving (Generic,Show)

instance ParseRecord Action


main :: IO ()
main = do
  command <- getRecord "automata equivalence"
  case command of
    NfaEquivalence _ -> printErrorAndExit "Checking NFA equivalence is not supported yet."
    DfaEquivalence filename -> do
      dfa <- parseInput filename
      let result =
            dfaStatesEquivalentHk dfa
                                  (dfaInitialState dfa)
                                  (dfaInitialState dfa)
      case result of
        Left (NotDfaState s) -> printErrorAndExit $ "The following is not a DFA state: " ++ show s
        Right equivalent ->
          do putStrLn (show equivalent)
             if equivalent
                then exitSuccess
                else exitWith (ExitFailure 1)

parseInput :: Maybe String -> IO (Dfa Char)
parseInput = maybe (parseInput' stdin) $ \file -> withFile file ReadMode parseInput'
  where
    parseInput' :: Handle -> IO (Dfa Char)
    parseInput' stream = do
      fileContents <- hGetContents stream
      either printErrorAndExit return $ do
        Result transitions acceptingStates checks <- parseHknt fileContents
        compileHkntToDfa transitions acceptingStates

printErrorAndExit msg = hPutStrLn stderr msg >> exitWith (ExitFailure 2)
