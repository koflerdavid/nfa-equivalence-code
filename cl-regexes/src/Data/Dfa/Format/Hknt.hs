{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Dfa.Format.Hknt
    ( NameGenerator
    , toHknt
    ) where

import           Data.FiniteAutomaton

import           Control.Monad           ( forM_, when )
import           Control.Monad.Trans.RWS
import           Data.Bifunctor          ( second )
import qualified Data.Map                as Map
import           Data.Monoid             ( (<>) )
import qualified Data.Set                as Set
import qualified Data.Text               as T
import           Data.Text.Lazy          ( toStrict )
import qualified Data.Text.Lazy.Builder  as TB

type NameGenerator s q a = (s, q -> s -> (s, a))

toHknt :: (Ord q, IsBoolean o, FiniteAutomaton m q Char o) => m -> NameGenerator s q T.Text -> T.Text
toHknt fa (initialGeneratorState, nextName) =
    toStrict . TB.toLazyText . snd $ execRWS toHknt' () (Map.empty, initialGeneratorState)
  where
    toHknt' = do
        forM_ (faStates fa) $ \from ->
            forM_ (Map.toList $ faTransitions fa from) $ \(input, to) -> do
                tell =<< state (getNameFor from)
                tell (" -" <> TB.singleton input <> TB.fromText "-> ")
                printListOfStates to
                tell "\n"

        let acceptingStates = faAcceptingStates fa
        when ((not . Set.null) acceptingStates) $ do
            tell "accept:"
            printListOfStates acceptingStates

    printListOfStates (Set.minView -> Just (minimalState, rest)) = do
        tell =<< state (getNameFor minimalState)
        forM_ (Set.toAscList rest) $ \stateToPrint -> do
            tell " "
            tell =<< state (getNameFor stateToPrint)
    printListOfStates (Set.minView -> _) = tell ""

    getNameFor automataState oldState@(names, generatorState) =
        case names Map.!? automataState of
            Just name -> (name, oldState)
            Nothing ->
                let (nextState, newName) =
                        second TB.fromText (nextName automataState generatorState)
                in (newName, (Map.insert automataState newName names, nextState))
