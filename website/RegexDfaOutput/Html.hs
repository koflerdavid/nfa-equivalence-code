{-# LANGUAGE OverloadedStrings #-}

module RegexDfaOutput.Html
  ( printTransitionTable
  , formatTransitionTable
  ) where

import           Algorithm.Regex.DfaConversion ( RegexDfaTransitions )

import           Control.Monad     (forM_)
import           Data.Map          as Map
import           Data.Regex
import           Data.Set          as Set
import           Data.Text.Lazy
import qualified Data.Text.Lazy.IO as TIO
import           Lucid

printTransitionTable :: Bool -> Regex Char -> RegexDfaTransitions -> IO ()
printTransitionTable withoutSkeleton regex transitions = do
  TIO.putStrLn (formatTransitionTable withoutSkeleton regex transitions)

formatTransitionTable :: Bool -> Regex Char -> RegexDfaTransitions -> Text
formatTransitionTable withoutSkeleton regex transitions = renderText $
  if withoutSkeleton
    then transitionTable
    else doctypehtml_ $ do
           head_ $ do
             meta_ [charset_ "UTF-8"]
             title_ "Regex to Dfa"
             style_ $ "td { border: 1px solid black; }"
           body_ $ do
             p_ $ do
               "The following is the state transition table for an "
               "automata accepting the same language as the regular expression "
               code_ $ toHtml . show $ regex
             p_ $ do
               "It was calculated by computing all the derivatives "
               "of the regular expression."
             transitionTable
  where
    transitionTable :: Html ()
    transitionTable =
      table_ $ do
        thead_ $
          tr_ $ do
            th_ "state"
            th_ "initial state?"
            th_ "final state?"
            forM_ (Set.toAscList $ alphabet regex) $ \c -> do
              th_ (toHtml . show $ c)
        tbody_ $
          forM_ (Map.toList transitions) $ \(r, ts) -> do
            tr_ $ do
              td_ $ toHtml . show $ r
              td_ $
                if regex == r
                  then "->"
                  else " "
              td_ $
                if matchesEmptyWord r
                  then "*"
                  else " "
              forM_ (Map.elems ts) $ td_ . toHtml . show
