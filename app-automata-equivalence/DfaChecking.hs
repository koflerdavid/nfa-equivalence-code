module DfaChecking
    ( checkDfaEquivalence
    ) where

import Control.Monad                      ( forM, forM_, when )
import Control.Monad.Trans.Class          ( lift )
import Control.Monad.Trans.Except         ( ExceptT, runExceptT, throwE )
import Data.Bimap                         as Bimap
import Data.List                          as List
import System.IO

import Algorithm.DfaEquivalence
import Compiler.Hknt
import Data.Dfa
import Language.Automata.HkntParser

type IOWithError a = ExceptT String IO a

checkDfaEquivalence :: Maybe String -> IOWithError [Bool]
checkDfaEquivalence filename = do
    (dfa, stateMapping, checks) <- parseInput filename
    forM (equivalenceChecks checks) $ \(stateSet1, _, stateSet2) -> do
        ensureSingletonStateSet stateSet1
        ensureSingletonStateSet stateSet2
        let (state1, state2) = (head stateSet1, head stateSet2)
        lift $
            hPutStrLn stderr $
            "Checking equivalence of " ++ state1 ++ " and " ++ state2
        state1' <- translateState stateMapping state1
        state2' <- translateState stateMapping state2
        let result = dfaStatesDifferencesHk dfa (Just state1') (Just state2')
        case result of
            Left (NotDfaState s) ->
                throwE $ "The following is not a DFA state: " ++ show s
            Right (maybeWitness, trace) -> do
                forM_ trace (printConstraint stateMapping)
                case maybeWitness of
                    Nothing -> return True
                    Just witness -> do
                        lift $ putStrLn "\nFailed on:"
                        printConstraint stateMapping (False, witness)
                        return False

printConstraint :: Bimap String Int -> (Bool, Constraint Char) -> IOWithError ()
printConstraint stateMapping (skipped, (w, x, y)) = do
    x' <- maybe (return "_|_") (translateState . Bimap.twist $ stateMapping) x
    y' <- maybe (return "_|_") (translateState . Bimap.twist $ stateMapping) y
    lift $ do
        when skipped $ putStr "skipped"
        putChar '\t'
        putStr (show w)
        putChar '\t'
        putStr x'
        putChar '\t'
        putStr y'
        putChar '\n'

parseInput ::
       Maybe String
    -> IOWithError (Dfa Char, Bimap String Int, [Check String])
parseInput =
    maybe (relift $ parseInput' stdin) $ \file ->
        relift $ withFile file ReadMode parseInput'
  where
    parseInput' ::
           Handle
        -> IO (Either String (Dfa Char, Bimap String Int, [Check String]))
    parseInput' stream =
        runExceptT $ do
            fileContents <- lift $ hGetContents stream
            either throwE return $ do
                Result transitions acceptingStates checks <-
                    parseHknt fileContents
                (dfa, stateMapping) <-
                    compileHkntToDfa transitions acceptingStates
                return (dfa, stateMapping, checks)
    relift :: IO (Either String a) -> IOWithError a
    relift m = lift m >>= either throwE return

equivalenceChecks :: [Check String] -> [Check String]
equivalenceChecks = List.filter (\(_, operation, _) -> operation == Equivalence)

translateState :: (Ord s, Ord s', Show s) => Bimap s s' -> s -> IOWithError s'
translateState stateMapping state =
    maybe (throwE $ show state ++ " does not exist") return $
    state `Bimap.lookup` stateMapping

ensureSingletonStateSet :: [s] -> IOWithError ()
ensureSingletonStateSet stateSet =
    when (length stateSet > 1) $ do
        throwE
            "Use the NFA mode if the equivalence of sets of states shall be checked."
