{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where


import Data.Dfa
import Data.Nfa
import Data.Regex

import Control.Monad (forM_, when)
import qualified Data.Map as Map
import Options.Generic
import System.Exit
import System.IO

import Algorithm.DfaEquivalence
import Compiler.Hknt
import Language.Automata.HkntParser.Class
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
      (dfa, stateMapping, checks) <- parseInput filename
      forM_ (equivalenceChecks checks) $ \(stateSet1, _, stateSet2) -> do
        ensureSingletonStateSet stateSet1
        ensureSingletonStateSet stateSet2
        let (state1, state2) =
                (head stateSet1, head stateSet2)
        hPutStrLn stderr $ "Checking equivalence of " ++ state1 ++ " and " ++ state2
        state1' <- translateState stateMapping state1
        state2' <- translateState stateMapping state2
        let result =
              dfaStatesEquivalentHk dfa (Just state1') (Just state2')
        case result of
          Left (NotDfaState s) -> printErrorAndExit $ "The following is not a DFA state: " ++ show s
          Right equivalent ->
            do putStrLn (show equivalent)
               if equivalent
                  then exitSuccess
                  else exitWith (ExitFailure 1)

parseInput :: Maybe String -> IO (Dfa Char, Map.Map String Int, [Check String])
parseInput = maybe (parseInput' stdin) $ \file -> withFile file ReadMode parseInput'
  where
--    parseInput' :: Handle -> IO (Dfa Char, [Check String])
    parseInput' stream = do
      fileContents <- hGetContents stream
      either printErrorAndExit return $ do
        Result transitions acceptingStates checks <- parseHknt fileContents
        (dfa, stateMapping) <- compileHkntToDfa transitions acceptingStates
        return (dfa, stateMapping, checks)

equivalenceChecks :: [Check String] -> [Check String]
equivalenceChecks = filter (\(_, operation, _) -> operation == Equivalence)

translateState :: (Ord s, Show s) => Map.Map s s' -> s -> IO s'
translateState stateMapping state =
    maybe (printErrorAndExit $ show state ++ " does not exist") return $ state `Map.lookup` stateMapping

ensureSingletonStateSet :: [s] -> IO ()
ensureSingletonStateSet stateSet =
    when (length stateSet > 1) $ do
        printErrorAndExit "Use the NFA mode if the equivalence of sets of states shall be checked."

printErrorAndExit msg = hPutStrLn stderr msg >> exitWith (ExitFailure 2)
