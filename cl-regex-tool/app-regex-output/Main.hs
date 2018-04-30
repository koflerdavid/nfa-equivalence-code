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

import           Algorithm.Regex.DfaConversion
import           Data.FiniteAutomaton.Format.DotGraph
import           Data.Regex.Formats                   ( toMinimallyQuotedString )
import           Language.RegexParser

import           Control.Monad                        ( void )
import           Data.GraphViz.Commands
import           Data.Maybe                           ( fromMaybe )
import qualified Data.Text.IO                         as TIO
import           Data.Text.Lazy                       as TL
import           Options.Generic
import           System.Exit                          ( exitFailure )
import           System.IO                            ( hPutStrLn, stderr )
import           Type.Reflection

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
    input <- TIO.getContents
    case parseRegex "<stdin>" input of
        Left e -> hPutStrLn stderr e >> exitFailure
        Right regex -> do
            let regexDfa = fromRegex regex
                statePrinter = TL.pack . toMinimallyQuotedString
                dotGraph = toDotGraphWithStatePrinter statePrinter regexDfa
                command = commandFor dotGraph
            case outputFormat of
                GraphvizCanvas -> runGraphvizCanvas command dotGraph Gtk
                Graphviz {file = maybePath, format = maybeFormat} ->
                    void $ runGraphviz
                               dotGraph
                               (fromMaybe Png maybeFormat)
                               (fromMaybe "/dev/stdout" maybePath)
