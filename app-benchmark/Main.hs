{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass     #-}

module Main where

import           Algorithm.NfaEquivalence
import           Data.Nfa

import           AutomataOutput
import qualified Interpolator             as I
import           RandomNfa

import           Control.Monad            ( forM, forM_ )
import           Control.DeepSeq          ( NFData, force )
import           Control.Monad.Random     ( evalRandT )
import           Criterion.Main
import qualified Data.IntSet              as IntSet
import           Data.IORef
import           Data.Maybe               ( isNothing )
import qualified Data.Text.Lazy.IO        as TIO
import           Options.Generic
import           System.Exit
import           System.IO
import           System.Random            ( getStdGen )

deriving instance Generic (Nfa c)
deriving instance NFData c => NFData (Nfa c)

automataSize :: Int
automataSize = 20

automataCount :: Int
automataCount = 20

transitionDensity :: Float
transitionDensity = 1.25

outputFileTemplate :: I.Template Char
outputFileTemplate = I.compile "automata_%%.dot"

main :: IO ()
main = do
    -- Use `nf` to avoid any interference of automata generation later on.
    automata <- fmap force (generateAutomata automataCount automataSize)
    timings <- newIORef [] :: IO (IORef [(Int, Bool, Int)])
    defaultMain [ bgroup "nfaStatesDifferencesHkC" $ map (createBench timings) automata ]
    printAutomata automata outputFileTemplate

createBench :: IORef [(Int, Bool, Int)] -> (Int, Nfa Char) -> Benchmark
createBench timings item@(i, _) =
    bench (show i) $ nfIO $ runTest timings item

runTest :: IORef [(Int, Bool, Int)] -> (Int, Nfa Char) -> IO (Bool, Int)
runTest refTimings (i, nfa) = do
    let stateSet1 = IntSet.singleton 0
        stateSet2 = IntSet.singleton 1
        (result, trace) = nfaStatesDifferencesHkC nfa stateSet1 stateSet2
        areStateSetsEqual = isNothing result
        processedPairsCount = length trace
    -- Store the number of processed pairs.
    modifyIORef' refTimings
                 ((i, areStateSetsEqual, processedPairsCount) :)
    return (areStateSetsEqual, processedPairsCount)

printProcessedPairsCount :: [(Int, Bool, Int)] -> IO ()
printProcessedPairsCount counts =
    forM_ counts $
        \(n, areStateSetsEqual, processedPairsCount) -> do
            putStr (show n)
            putChar ';'
            -- Display whether the check was successful or not.
            -- This is interesting because parts of the automata might be unreachable.
            putStr (show areStateSetsEqual)
            putChar ';'
            putStr (show processedPairsCount)
            putChar '\n'

generateAutomata :: Int -> Int -> IO [(Int, Nfa Char)]
generateAutomata numberOfAutomata numberOfStates = do
    generator <- getStdGen
    (`evalRandT` generator) $ do
        forM [1 .. numberOfStates] $
            \i -> (,) <$> pure i <*> randomNfa numberOfStates [ 'a', 'b' ] transitionDensity 0.0

printAutomata :: [(Int, Nfa Char)] -> I.Template Char -> IO ()
printAutomata automata template =
    forM_ automata $
        \(i, nfa) -> do
            let renderedNfaGraph = renderAutomata nfa
                name = I.unsafeInterpolate template [ (show i) ]
            TIO.writeFile name renderedNfaGraph

printErrorAndExit :: String -> IO a
printErrorAndExit msg = hPutStrLn stderr msg >> exitWith (ExitFailure 2)
