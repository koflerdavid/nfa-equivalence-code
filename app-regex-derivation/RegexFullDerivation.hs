module RegexFullDerivation where

import qualified RegexDfaOutput.LaTeX       as LaTeXOutput
import qualified RegexDfaOutput.Tsv         as TsvOutput
import           Types

import           Algorithm.Regex.Derivation
import           Data.Queue                 as Queue
import           Data.Regex
import           Language.RegexParser

import           Control.Monad.Trans.Class  ( lift )
import           Control.Monad.Trans.Except ( ExceptT, throwE )
import           Control.Monad.Trans.State  ( State, evalState, gets, modify )
import           Data.List                  as List
import           Data.Map                   as Map
import           Data.Set                   as Set

parseAndDeriveRegexToDfa :: OutputFormat -> Bool -> ExceptT String IO ()
parseAndDeriveRegexToDfa outputFormat withoutSkeleton = do
    input <- lift $ getContents
    case parseRegex "<stdin>" input of
        Left parseError -> throwE parseError
        Right regex -> do
            let printer = case outputFormat of
                    Latex -> LaTeXOutput.printTransitionTable
                    Tsv -> TsvOutput.printTransitionTable
            lift $ printer withoutSkeleton regex (deriveRegexToDfa regex)

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
deriveRegexForAlphabet regexAlphabet regex =
    modify $
        \(transitions, queue) ->
            let derivations = List.map (\c -> (c, derive c regex)) regexAlphabet
                regexesToQueue = List.filter (`Map.notMember` transitions) $ snd (unzip derivations)
                transitions' = Map.insert regex (Map.fromAscList derivations) transitions
            in
                (transitions', Queue.pushAll queue regexesToQueue)
