module RegexEquivalence where

import           Algorithm.Regex.Equivalence
import           Language.RegexParser

import           Control.Monad               ( forM_ )
import           Control.Monad.Trans.Class   ( lift )
import           Control.Monad.Trans.Except  ( ExceptT, throwE )

checkRegexEquivalence :: ExceptT String IO Bool
checkRegexEquivalence = do
    regex1 <- lift getLine >>= (exceptM . parseRegex "first regex")
    regex2 <- lift getLine >>= (exceptM . parseRegex "second regex")

    let witnesses = getDifferences regex1 regex2
    lift $
        forM_ witnesses $
            \(w, r1, r2) -> do
                putStr (show w)
                putChar '\t'
                putStr (show r1)
                putChar '\t'
                putStrLn (show r2)
    return (null witnesses)

exceptM :: Monad m => Either e a -> ExceptT e m a
exceptM (Left e) = throwE e
exceptM (Right a) = return a