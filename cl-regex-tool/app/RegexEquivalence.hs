module RegexEquivalence where

import           Algorithm.Regex.Equivalence
import           Data.Regex.Formats          ( MinimallyQuotedRegex (..) )
import           Language.RegexParser

import           Control.Monad               ( forM_ )
import           Control.Monad.Trans.Class   ( lift )
import           Control.Monad.Trans.Except  ( ExceptT(..), throwE )
import qualified Data.Text.IO                as TIO

checkRegexEquivalence :: ExceptT String IO Bool
checkRegexEquivalence = do
    regex1 <- lift TIO.getLine >>= ExceptT . return . parseRegex "first regex"
    regex2 <- lift TIO.getLine >>= ExceptT . return . parseRegex "second regex"
    let (witnesses, _trace) = getDifferences regex1 regex2
    lift $
        forM_ witnesses $ \(w, r1, r2) -> do
            printSameLine w
            putChar '\t'
            printSameLine . MinimallyQuotedRegex $ r1
            putChar '\t'
            print . MinimallyQuotedRegex $ r2
    return (null witnesses)

exceptM :: Monad m => Either e a -> ExceptT e m a
exceptM (Left e)  = throwE e
exceptM (Right a) = return a

printSameLine :: Show a => a -> IO ()
printSameLine = putStr . show