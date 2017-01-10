module NfaChecking ( checkNfaEquivalence ) where

import           Control.Monad                      ( forM )
import           Control.Monad.Trans.Class          ( lift )
import           Control.Monad.Trans.Except         ( ExceptT, runExceptT, throwE )
import           Data.IntSet                        as IS
import           Data.List                          as List
import           Data.Map                           as Map
import           System.IO

import           Algorithm.NfaEquivalence
import           Compiler.Hknt
import           Data.Nfa
import           Language.Automata.HkntParser.Class
import           Language.Automata.HkntParser

type IOWithError a = ExceptT String IO a

checkNfaEquivalence :: Maybe String -> IOWithError [Bool]
checkNfaEquivalence filename = do
    (nfa, stateMapping, checks) <- parseInput filename
    forM (equivalenceChecks checks) $
        \(stateSet1, _, stateSet2) -> do
            let (strStateSet1, strStateSet2) =
                    (show stateSet1, show stateSet2)
            lift $
                hPutStrLn stderr $
                    "Checking equivalence of " ++ strStateSet1 ++ " and " ++ strStateSet2
            stateSet1' <- mapM (translateState stateMapping) stateSet1
            stateSet2' <- mapM (translateState stateMapping) stateSet2
            let result = nfaStatesEquivalentHkC nfa
                                                (IS.fromList stateSet1')
                                                (IS.fromList stateSet2')
            lift $ putStrLn (show result)
            return result

parseInput :: Maybe String -> IOWithError (Nfa Char, Map.Map String Int, [Check String])
parseInput = maybe (relift $ parseInput' stdin) $
    \file -> relift $ withFile file ReadMode parseInput'
  where
    parseInput' :: Handle -> IO (Either String (Nfa Char, Map.Map String Int, [Check String]))
    parseInput' stream = runExceptT $ do
        fileContents <- lift $ hGetContents stream
        either throwE return $ do
            Result transitions acceptingStates checks <- parseHknt fileContents
            (nfa, stateMapping) <- compileHkntToNfa transitions acceptingStates
            return (nfa, stateMapping, checks)

    relift :: IO (Either String a) -> IOWithError a
    relift m = lift m >>= either throwE return

equivalenceChecks :: [Check String] -> [Check String]
equivalenceChecks = List.filter (\(_, operation, _) -> operation == Equivalence)

translateState :: (Ord s, Show s) => Map.Map s s' -> s -> IOWithError s'
translateState stateMapping state =
    maybe (throwE $ show state ++ " does not exist") return $
        state `Map.lookup` stateMapping
