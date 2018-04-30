module RegexFullDerivation where

import qualified RegexDfaOutput.Html        as HtmlOutput
import qualified RegexDfaOutput.LaTeX       as LaTeXOutput
import qualified RegexDfaOutput.Tsv         as TsvOutput
import           Types

import           Language.RegexParser

import           Control.Exception.Safe     ( throw )
import qualified Data.Text.IO               as TIO

parseAndDeriveRegexToDfa :: OutputFormat -> Bool -> IO ()
parseAndDeriveRegexToDfa outputFormat withoutSkeleton = do
    input <- TIO.getContents
    case parseRegex "<stdin>" input of
        Left parseError -> throw (RegexParseException parseError)
        Right regex -> do
            let printer =
                    case outputFormat of
                        Html  -> HtmlOutput.printTransitionTable
                        Latex -> LaTeXOutput.printTransitionTable
                        Tsv   -> TsvOutput.printTransitionTable
            printer withoutSkeleton regex
