{-# LANGUAGE ExistentialQuantification #-}

module DfaChecking
    ( DfaCheckingException
    , checkDfaEquivalence
    ) where

import           Common

import           Algorithm.DfaEquivalence
import           Compiler.Hknt
import           Data.Dfa
import           Language.Automata.HkntParser

import           Control.Monad.Catch          ( Exception, MonadThrow, handle, throwM )
import           Control.Monad                ( forM, forM_, when )
import           Data.Bimap                   as Bimap
import           Data.List                    as List
import           Data.Maybe                   ( fromMaybe )
import qualified Data.Text.IO                 as TIO
import           System.IO                    ( IOMode (ReadMode), hPutStrLn,
                                                stderr, stdin, withFile )

data DfaCheckingException
    = NotADfaState String
    | NotASingletonStateSet [String]
    | StateDoesNotExist String
    | CouldNotTranslateStateBack DfaState
    | HkntSyntaxError String
    | HkntToDfaTranslationError HkntCompileError
    deriving (Show)

instance Exception DfaCheckingException

checkDfaEquivalence :: Maybe String -> IO [Bool]
checkDfaEquivalence filename = do
    (dfa, stateMapping, checks) <- parseInput filename
    forM (equivalenceChecks checks) $ \(stateSet1, _, stateSet2) -> do
        ensureSingletonStateSet stateSet1
        ensureSingletonStateSet stateSet2
        let (state1, state2) = (head stateSet1, head stateSet2)
        hPutStrLn stderr ("Checking equivalence of " ++ state1 ++ " and " ++ state2)
        state1' <- translateState stateMapping state1
        state2' <- translateState stateMapping state2
        let (maybeWitness, trace) = dfaStatesDifferencesHk dfa state1' state2'
        forM_ trace (printConstraint stateMapping)
        case maybeWitness of
            Nothing -> return True
            Just witness -> do
                putStrLn "\nFailed on:"
                printConstraint stateMapping (False, witness)
                return False

printConstraint :: Bimap String DfaState -> (Bool, Constraint Char) -> IO ()
printConstraint stateMapping (skipped, (w, x, y)) = do
    x' <- if x == dfaErrorState then return "_|_" else translateBack stateMapping x
    y' <- if x == dfaErrorState then return "_|_" else translateBack stateMapping y

    when skipped $ putStr "skipped"
    putChar '\t'
    putStr (show w)
    putChar '\t'
    putStr x'
    putChar '\t'
    putStr y'
    putChar '\n'

parseInput :: Maybe String -> IO (Dfa Char, Bimap String DfaState, [Check String])
parseInput maybeFilePath =
    case maybeFilePath of
        Just filePath -> withFile filePath ReadMode parseInput'
        Nothing       -> parseInput' stdin
  where
    parseInput' stream = do
        fileContents <- TIO.hGetContents stream
        Result transitions acceptingStates checks <-
            onLeftThrow HkntSyntaxError $ parseHknt fileContents
        (dfa, stateMapping) <-
            onLeftThrow HkntToDfaTranslationError $ compileHkntToDfa transitions acceptingStates
        return (dfa, stateMapping, checks)

equivalenceChecks :: [Check String] -> [Check String]
equivalenceChecks = List.filter (\(_, operation, _) -> operation == Equivalence)

ensureSingletonStateSet :: [String] -> IO ()
ensureSingletonStateSet stateSet =
    when (length stateSet > 1) $ throwM (NotASingletonStateSet stateSet)

translateState :: MonadThrow m => Bimap String DfaState -> String -> m DfaState
translateState stateMapping state =
    onNothingThrow (StateDoesNotExist state) $ state `Bimap.lookup` stateMapping

translateBack :: MonadThrow m => Bimap String DfaState -> DfaState -> m String
translateBack stateMapping state =
    onNothingThrow (CouldNotTranslateStateBack state) $ state `Bimap.lookupR` stateMapping
