{-# LANGUAGE ScopedTypeVariables #-}

module Algorithm.Regex.DfaConversion
    ( deriveRegexToDfa
    ) where

import Algorithm.Regex.Derivation ( derive )
import Data.Queue                 as Queue
import Data.Regex                 ( Regex, alphabet )

import Control.Monad.Trans.State  ( State, evalState, gets, modify )
import Data.List                  as List
import Data.Map                   as Map
import Data.Set                   as Set

type RegexDfaTransitions c = Map (Regex c) (Map c (Regex c))

deriveRegexToDfa :: Ord c => Regex c -> RegexDfaTransitions c
deriveRegexToDfa regex =
    let queue = FifoQueue [] `Queue.push` regex
    in evalState processQueue (Map.empty, queue)
  where
    regexAlphabet = Set.toAscList (alphabet regex)
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
deriveRegexForAlphabet ::
       (Ord c, Queue q)
    => [c]
    -> Regex c
    -> State (RegexDfaTransitions c, q (Regex c)) ()
deriveRegexForAlphabet regexAlphabet regex =
    modify $ \(transitions, queue) ->
        let derivations = List.map (\c -> (c, derive c regex)) regexAlphabet
            regexesToQueue =
                List.filter (`Map.notMember` transitions) $
                List.map snd derivations
            transitions' =
                Map.insert regex (Map.fromAscList derivations) transitions
        in (transitions', Queue.pushAll queue regexesToQueue)
