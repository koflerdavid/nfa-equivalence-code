{-# LANGUAGE OverloadedStrings #-}

module Data.Dfa.Format.Html ( asHtml ) where

import           Data.Dfa.Regex ( RegexDfaTransitions )
import           Data.Regex

import           Control.Monad     ( forM_ )
import qualified Data.Text.Lazy as T
import           Data.Map       as Map
import           Data.Set       as Set
import           Lucid

asHtml :: Bool -> Regex Char -> RegexDfaTransitions -> T.Text
asHtml withoutSkeleton regex transitions =
    renderText $
        if withoutSkeleton
        then transitionTable
        else doctypehtml_ $ do
            head_ $ do
                meta_ [ charset_ "UTF-8" ]
                title_ "Regex to Dfa"
                style_ "td { border: 1px solid black; }"
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
    transitionTable = table_ $ do
        thead_ $
            tr_ $ do
                th_ "state"
                th_ "initial state?"
                th_ "final state?"
                forM_ (Set.toAscList $ alphabet regex) $
                    \c ->
                        th_ (toHtml . show $ c)
        tbody_ $
            forM_ (Map.toList transitions) $
                \(r, ts) ->
                    tr_ $ do
                        td_ $ toHtml . show $ r
                        td_ $ if regex == r then "->" else " "
                        td_ $ if matchesEmptyWord r then "*" else " "
                        forM_ (Map.elems ts) $ td_ . toHtml . show
