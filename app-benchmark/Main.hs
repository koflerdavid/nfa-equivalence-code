{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Algorithm.NfaEquivalence

import           AutomataOutput
import qualified Interpolator as I
import           RandomNfa

import           Control.Monad              ( forM, forM_, when )
import           Control.Monad.Random       ( evalRandT )
import           Control.Monad.Trans.Class  ( lift )
import           Control.Monad.Trans.Except ( runExceptT )
import qualified Data.IntSet                as IntSet
import qualified Data.List                  as List
import           Data.Time.Clock            ( diffTimeToPicoseconds, diffUTCTime, getCurrentTime )
import           Data.Maybe                 ( isNothing )
import qualified Data.Text.Lazy.IO          as TIO
import           Options.Generic
import           System.Exit
import           System.IO
import           System.Random              ( getStdGen )

data Action = Benchmark { size       :: Int
                        , tries      :: Int
                        , treshold   :: Maybe Int
                        , outputFile :: Maybe String
                        }
    deriving (Generic, Show)

instance ParseRecord Action

picosecondsPerMilliSecond :: Integer
picosecondsPerMilliSecond =
    10 ^ 9

main :: IO ()
main = do
    command <- getRecord "benchmark"
    let n = size command
        triesCount = tries command
        outputFileTemplate = fmap I.compile (outputFile command)
    when (n < 2) $ printErrorAndExit "At least one state is needed"
    generator <- getStdGen
    results <- (`evalRandT` generator) $
                   forM [1 .. triesCount] $
                       \i -> do
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
                           lift $ do
                               let renderedNfaGraph = renderAutomata nfa
                               case outputFileTemplate of
                                   Nothing -> TIO.hPutStrLn stderr renderedNfaGraph
                                   Just template -> do
                                       let name = I.unsafeInterpolate template [(show i)]
                                       TIO.writeFile name renderedNfaGraph
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
