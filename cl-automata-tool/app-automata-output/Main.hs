{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{- |
Module:      Main
Description: 
Copyright:   (C) David Kofler
License:     BSD3 (see the LICENSE file in the distribution)

Maintainer:  kofler.david@gmail.com
Stability:   provisional
Portability: portable (Haskell 2010)


-}
module Main where

import Common

import Compiler.Hknt
import Data.FiniteAutomaton.Format.DotGraph
import Data.Nfa
import Language.Automata.HkntParser

import Control.Monad                        ( void )
import Control.Monad.Catch                  ( Exception )
import Data.Bimap                           as Bimap
import Data.GraphViz.Commands
import Data.Maybe                           ( fromMaybe )
import Data.Text.IO                         as TIO
import Data.Text.Lazy                       as TL
import Options.Generic
import System.IO                            ( IOMode (..), stdin, withFile )
import Type.Reflection

data OutputFormat
    = GraphvizCanvas
    | Graphviz { file   :: !(Maybe FilePath)
               , format :: !(Maybe GraphvizOutput) }
    deriving (Generic, Show)

deriving instance Typeable GraphvizOutput

instance ParseRecord OutputFormat
instance ParseField GraphvizOutput

main :: IO ()
main = do
    outputFormat <- getRecord "automata output"
    (nfa, mapping, _checks) <- parseInputToNfa Nothing
    let statePrinter = TL.pack . (mapping Bimap.!>)
        dotGraph = toDotGraphWithStatePrinter statePrinter nfa
        command = (commandFor dotGraph)
    case outputFormat of
        GraphvizCanvas -> runGraphvizCanvas command dotGraph Gtk
        Graphviz {file = maybePath, format = maybeFormat} ->
            void $ runGraphviz dotGraph
                               (fromMaybe Png maybeFormat)
                               (fromMaybe "/dev/stdout" maybePath)

data NfaParsingException
    = StateDoesNotExist String
    | CouldNotTranslateStateBack Int
    | HkntSyntaxError String
    | HkntToNfaTranslationError HkntCompileError
    deriving (Show)

instance Exception NfaParsingException

parseInputToNfa :: Maybe String -> IO (Nfa Char, Bimap String Int, [Check String])
parseInputToNfa maybeFilePath =
    case maybeFilePath of
        Just filePath -> withFile filePath ReadMode parseInput'
        Nothing       -> parseInput' stdin
  where
    parseInput' stream = do
        fileContents <- TIO.hGetContents stream
        Result transitions acceptingStates checks <-
            onLeftThrow HkntSyntaxError $ parseHknt fileContents
        (nfa, stateMapping) <-
            onLeftThrow HkntToNfaTranslationError $ compileHkntToNfa transitions acceptingStates
        return (nfa, stateMapping, checks)
