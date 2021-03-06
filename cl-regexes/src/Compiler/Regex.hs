module Compiler.Regex
    ( compileRegex
    ) where

import Data.EpsilonNfa                  as ENfa
import Data.Regex

import Control.Monad.Trans.State.Strict
import Data.Foldable                    ( toList )
import Data.Sequence                    as Seq hiding ( Empty )

type FaState = Int

compileRegex :: Ord c => Regex c -> ENfa.ENfa c
compileRegex regex = ENfa.buildEnfa [1] (toList transitions)
  where
    (_lastFaState, transitions) =
        execState (compileRegex' regex 0 1) (2, Seq.empty)

type Transition c = ((FaState, Maybe c), [FaState])

-- The compiler FaState consists of a start FaState and a counter to generate fresh
-- FaStates and it remembers all generated transitions
type CompilerState c a = State (FaState, Seq (Transition c)) a

-- The compiler generates the automaton between the two specified FaStates.
compileRegex' :: Regex c -> FaState -> FaState -> CompilerState c ()
compileRegex' (Atom c) startFaState endFaState =
    transition startFaState c [endFaState]
-- Just don't connect the given FaStates to each other
compileRegex' Empty _startFaState _endFaState = return ()
compileRegex' (Alternative r s) startFaState endFaState = do
    mapM_ (\t -> compileRegex' t startFaState endFaState) [r, s]
-- generate an automaton with just one transition. Unfortunately it is quite difficult to avoid creating
-- this intermediary FaState, but it's not too bad. Many cases can be avoided with the use of `Optional`.
compileRegex' Epsilon startFaState endFaState =
    startFaState `epsilonTransitionsTo` [endFaState]
compileRegex' (Sequence first second) startFaState finalFaState =
    compileSequence first [second] startFaState
    -- if there is just one element then we have to stop because the last FaState is already given
  where
    compileSequence r [] currentStartFaState =
        compileRegex' r currentStartFaState finalFaState
    compileSequence r (r':rs') currentStartFaState = do
        endFaState <- freshFaState
        compileRegex' r currentStartFaState endFaState
        compileSequence r' rs' endFaState
compileRegex' (Asterisk inner) startFaState endFaState = do
    compileRegex' inner startFaState endFaState
    startFaState `epsilonTransitionsTo` [endFaState]
    endFaState `epsilonTransitionsTo` [startFaState]

freshFaState :: CompilerState c FaState
freshFaState = do
    (s, transitions) <- get
    put (succ s, transitions)
    return s

transition :: FaState -> c -> [FaState] -> CompilerState c ()
transition start c ends =
    modify $ \(nextVariable, transitions) ->
        (nextVariable, transitions |> ((start, Just c), ends))

-- meant to be used as an operator
epsilonTransitionsTo :: FaState -> [FaState] -> CompilerState c ()
start `epsilonTransitionsTo` ends =
    modify $ \(nextVariable, transitions) ->
        (nextVariable, transitions |> ((start, Nothing), ends))
