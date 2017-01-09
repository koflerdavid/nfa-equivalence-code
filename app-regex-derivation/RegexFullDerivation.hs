module RegexFullDerivation where

import           Algorithm.Regex.Derivation
import           Data.Queue                 as Queue
import           Data.Regex
import           Language.RegexParser
import qualified RegexDfaOutput.LaTeX       as LaTeXOutput
import           Types

import           Control.Monad
import           Control.Monad.Trans.Class  ( lift )
import           Control.Monad.Trans.Except ( ExceptT, runExceptT, throwE )
import           Control.Monad.Trans.State
import           Data.List                  as List
import           Data.Map                   as Map
import           Data.Set                   as Set
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO
import           System.IO

parseAndDeriveRegexToDfa :: ExceptT String IO ()
parseAndDeriveRegexToDfa = do
    input <- lift $ getContents
    case parseRegex "<stdin>" input of
        Left parseError -> throwE parseError
        Right regex -> do
            lift $ LaTeXOutput.printTransitionTable regex (deriveRegexToDfa regex)

deriveRegexToDfa :: Regex Char -> RegexDfaTransitions
deriveRegexToDfa regex =
    let queue = Queue.empty `Queue.push` regex
    in
        evalState processQueue (Map.empty, queue)
  where
    regexAlphabet = Set.toAscList (alphabet regex)
    processQueue :: State (RegexDfaTransitions, FifoQueue (Regex Char)) RegexDfaTransitions
    processQueue = do
        queue <- gets snd
        case Queue.pop queue of
            Nothing -> gets fst -- return the transition table
            Just (r, queue') -> do
                modify (\(transitions, _) -> (transitions, queue'))
                deriveRegexForAlphabet regexAlphabet r
                processQueue

-- | For this to work, the alphabet must be ascending!
-- Also, all elements in the queue are assumed to not be already contained in the transition map
deriveRegexForAlphabet :: Queue q
                       => [Char]
                       -> Regex Char
                       -> State (RegexDfaTransitions, q (Regex Char)) ()
deriveRegexForAlphabet alphabet regex =
    modify $
        \(transitions, queue) ->
            let derivations = List.map (\c -> (c, derive c regex)) alphabet
                regexesToQueue = List.filter (`Map.notMember` transitions) $ snd (unzip derivations)
                transitions' = Map.insert regex (Map.fromAscList derivations) transitions
            in
                (transitions', Queue.pushAll queue regexesToQueue)
