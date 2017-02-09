{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Algorithm.NfaEquivalence

import           RandomNfa

import           Control.Monad              ( forM, forM_, when )
import           Control.Monad.Random       ( evalRandT )
import           Control.Monad.Trans.Class  ( lift )
import           Control.Monad.Trans.Except ( runExceptT )
import qualified Data.IntSet                as IntSet
import           Data.Time.Clock            ( diffTimeToPicoseconds, diffUTCTime, getCurrentTime )
import           Data.Maybe                 ( isNothing )
import           Options.Generic
import           System.Exit
import           System.IO
import           System.Random              ( getStdGen )

data Action = Benchmark Int Int
    deriving (Generic, Show)

size :: Action -> Int
size (Benchmark n _) = n

tries :: Action -> Int
tries (Benchmark _ t) = t

instance ParseRecord Action

picosecondsPerMilliSecond :: Integer
picosecondsPerMilliSecond =
    10 ^ 9

main :: IO ()
main = do
    command <- getRecord "benchmark"
    let n = size command
        triesCount = tries command
    when (n < 2) $ printErrorAndExit "At least one state is needed"
    generator <- getStdGen
    results <- (`evalRandT` generator) $
                   forM [0 .. triesCount] $
                       \_ -> do
                           start <- lift getCurrentTime
                           nfa <- randomNfa n [ 'a', 'b' ] 1.25 0.0
                           let stateSet1 = IntSet.singleton 0
                               stateSet2 = IntSet.singleton 1
                               (result, trace) = nfaStatesDifferencesHkC nfa stateSet1 stateSet2
                               processedPairsCount = length trace
--                           when (isNothing result) $
--                               lift $ printErrorAndExit "states should be equal"
                           -- Force the processed pair count to be processed before the end time is measured
                           end <- lift $ processedPairsCount `seq` getCurrentTime
                           let timeElapsed = (diffUTCTime end start)
                           return (timeElapsed, processedPairsCount)
    forM_ results $
        \(time, pairCount) -> do
            putStr (show n)
            putChar ';'
            putStr (show time)
            putChar ';'
            putStr (show pairCount)
            putChar '\n'

printErrorAndExit :: String -> IO a
printErrorAndExit msg = hPutStrLn stderr msg >> exitWith (ExitFailure 2)
