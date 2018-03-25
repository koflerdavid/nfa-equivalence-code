module RegexEquivalence
    ( checkRegexEquivalence
    ) where

import           Algorithm.Regex.Equivalence
import           Data.Regex.Formats          ( toMinimallyQuotedString )
import           Language.RegexParser
import           Types

import           Control.Monad               ( forM_ )
import           Control.Exception.Safe      ( Exception, MonadThrow, throw )
import qualified Data.Text.IO                as TIO

checkRegexEquivalence :: IO Bool
checkRegexEquivalence = do
    regex1 <- onLeftThrow RegexParseException . parseRegex "first regex"  =<< TIO.getLine
    regex2 <- onLeftThrow RegexParseException . parseRegex "second regex" =<< TIO.getLine
    let (witnesses, _trace) = getDifferences regex1 regex2

    forM_ witnesses $ \(w, r1, r2) -> do
        printSameLine w
        putChar '\t'
        putStr (toMinimallyQuotedString r1)
        putChar '\t'
        putStrLn (toMinimallyQuotedString r2)

    return (null witnesses)

onLeftThrow :: (Exception e, MonadThrow m) => (a -> e) -> Either a b -> m b
onLeftThrow toException = either (throw . toException) return

printSameLine :: Show a => a -> IO ()
printSameLine = putStr . show
