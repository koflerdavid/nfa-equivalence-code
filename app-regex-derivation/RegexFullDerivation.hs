module RegexFullDerivation where

import qualified RegexDfaOutput.Html        as HtmlOutput
import qualified RegexDfaOutput.LaTeX       as LaTeXOutput
import qualified RegexDfaOutput.Tsv         as TsvOutput
import           Types

import           Language.RegexParser

import           Control.Monad.Trans.Class  ( lift )
import           Control.Monad.Trans.Except ( ExceptT, throwE )

parseAndDeriveRegexToDfa :: OutputFormat -> Bool -> ExceptT String IO ()
parseAndDeriveRegexToDfa outputFormat withoutSkeleton = do
    input <- lift getContents
    case parseRegex "<stdin>" input of
        Left parseError -> throwE parseError
        Right regex -> do
            let printer =
                    case outputFormat of
                        Html  -> HtmlOutput.printTransitionTable
                        Latex -> LaTeXOutput.printTransitionTable
                        Tsv   -> TsvOutput.printTransitionTable
            lift $ printer withoutSkeleton regex
