{-# LANGUAGE ExistentialQuantification #-}

module NfaChecking
    ( NfaCheckingException
    , checkNfaEquivalence
    ) where

import           Common

import           Algorithm.NfaEquivalence
import           Compiler.Hknt
import           Data.Nfa
import           Language.Automata.HkntParser

import           Control.Monad                ( forM, forM_, mapM, when )
import           Control.Monad.Catch          ( Exception, MonadThrow )
import           Data.Bimap                   as Bimap hiding ( fromList,
                                                         toList )
import           Data.IntSet                  ( fromList, toList )
import           Data.List                    as List
import qualified Data.Text.IO                 as TIO
import           System.IO                    ( IOMode (ReadMode), hPutStrLn,
                                                stderr, stdin, withFile )

data NfaCheckingException
    = StateDoesNotExist String
    | CouldNotTranslateStateBack Int
    | HkntSyntaxError String
    | HkntToDfaTranslationError HkntCompileError
    deriving (Show)

instance Exception NfaCheckingException

checkNfaEquivalence :: Maybe String -> IO [Bool]
checkNfaEquivalence filename = do
    (nfa, stateMapping, checks) <- parseInput filename
    forM (equivalenceChecks checks) $ \(stateSet1, _, stateSet2) -> do
        let (strStateSet1, strStateSet2) = (show stateSet1, show stateSet2)
        hPutStrLn stderr ("Checking equivalence of " ++ strStateSet1 ++ " and " ++ strStateSet2)
        stateSet1' <- mapM (translateState stateMapping) stateSet1
        stateSet2' <- mapM (translateState stateMapping) stateSet2
        let (maybeWitness, trace) =
                nfaStatesDifferencesHkC nfa (fromList stateSet1') (fromList stateSet2')
        forM_ trace (printConstraint stateMapping)
        case maybeWitness of
            Nothing -> return True
            Just witness -> do
                putStrLn "\nFailed on:"
                printConstraint stateMapping (False, witness)
                return False

printConstraint :: Bimap String Int -> (Bool, Constraint Char) -> IO ()
printConstraint stateMapping (skipped, (w, xs, ys)) = do
    xs' <- mapM (translateBack stateMapping) (toList xs)
    ys' <- mapM (translateBack stateMapping) (toList ys)

    when skipped $ putStr "skipped"
    putChar '\t'
    putStr (show w)
    putChar '\t'
    putStr "{ "
    putStr (List.intercalate ", " xs')
    putStr " }\t{ "
    putStr (List.intercalate ", " ys')
    putStr " }\n"

parseInput :: Maybe String -> IO (Nfa Char, Bimap String Int, [Check String])
parseInput maybeFilePath =
    case maybeFilePath of
        Just filePath -> withFile filePath ReadMode parseInput'
        Nothing       -> parseInput' stdin
  where
    parseInput' stream = do
        fileContents <- TIO.hGetContents stream
        Result transitions acceptingStates checks <-
            onLeftThrow HkntSyntaxError $ parseHknt fileContents
        (nfa, stateMapping) <-
            onLeftThrow HkntToDfaTranslationError $ compileHkntToNfa transitions acceptingStates
        return (nfa, stateMapping, checks)

equivalenceChecks :: [Check String] -> [Check String]
equivalenceChecks = List.filter (\(_, operation, _) -> operation == Equivalence)

translateState :: MonadThrow m => Bimap String Int -> String -> m Int
translateState stateMapping state =
    onNothingThrow (StateDoesNotExist state) $ state `Bimap.lookup` stateMapping

translateBack :: MonadThrow m => Bimap String Int -> Int -> m String
translateBack stateMapping state =
    onNothingThrow (CouldNotTranslateStateBack state) $ state `Bimap.lookupR` stateMapping
