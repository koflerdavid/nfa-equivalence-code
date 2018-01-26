module NfaChecking
    ( checkNfaEquivalence
    ) where

import Control.Monad                ( forM, forM_, mapM, when )
import Control.Monad.Trans.Class    ( lift )
import Control.Monad.Trans.Except   ( ExceptT, runExceptT, throwE )
import Data.IntSet                  ( fromList, toList )
import Data.List                    as List
import Data.Map                     as Map hiding ( fromList, toList )
import System.IO

import Algorithm.NfaEquivalence
import Compiler.Hknt
import Data.Nfa
import Language.Automata.HkntParser

type IOWithError a = ExceptT String IO a

checkNfaEquivalence :: Maybe String -> IOWithError [Bool]
checkNfaEquivalence filename = do
    (nfa, stateMapping, checks) <- parseInput filename
    forM (equivalenceChecks checks) $ \(stateSet1, _, stateSet2) -> do
        let (strStateSet1, strStateSet2) = (show stateSet1, show stateSet2)
        lift $
            hPutStrLn stderr $
            "Checking equivalence of " ++
            strStateSet1 ++ " and " ++ strStateSet2
        stateSet1' <- mapM (translateState stateMapping) stateSet1
        stateSet2' <- mapM (translateState stateMapping) stateSet2
        let (maybeWitness, trace) =
                nfaStatesDifferencesHkC
                    nfa
                    (fromList stateSet1')
                    (fromList stateSet2')
        let invStateMapping = invertedStateMapping stateMapping
        forM_ trace (printConstraint invStateMapping)
        case maybeWitness of
            Nothing -> return True
            Just witness -> do
                lift $ putStrLn "\nFailed on:"
                printConstraint invStateMapping (False, witness)
                return False

printConstraint :: Map Int String -> (Bool, Constraint Char) -> IOWithError ()
printConstraint stateMapping (skipped, (w, xs, ys)) = do
    xs' <- mapM (translateState stateMapping) (toList xs)
    ys' <- mapM (translateState stateMapping) (toList ys)
    lift $ do
        when skipped $ putStr "skipped"
        putChar '\t'
        putStr (show w)
        putChar '\t'
        putStr "{ "
        putStr (List.intercalate ", " xs')
        putStr " }\t{ "
        putStr (List.intercalate ", " ys')
        putStr " }\n"

parseInput ::
       Maybe String
    -> IOWithError (Nfa Char, Map.Map String Int, [Check String])
parseInput =
    maybe (relift $ parseInput' stdin) $ \file ->
        relift $ withFile file ReadMode parseInput'
  where
    parseInput' stream =
        runExceptT $ do
            fileContents <- lift $ hGetContents stream
            either throwE return $ do
                Result transitions acceptingStates checks <-
                    parseHknt fileContents
                (nfa, stateMapping) <-
                    compileHkntToNfa transitions acceptingStates
                return (nfa, stateMapping, checks)
    relift :: IO (Either String a) -> IOWithError a
    relift m = lift m >>= either throwE return

equivalenceChecks :: [Check String] -> [Check String]
equivalenceChecks = List.filter (\(_, operation, _) -> operation == Equivalence)

translateState :: (Ord s, Show s) => Map.Map s s' -> s -> IOWithError s'
translateState stateMapping state =
    maybe (throwE $ show state ++ " does not exist") return $
    state `Map.lookup` stateMapping
