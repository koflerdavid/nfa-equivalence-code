{-# LANGUAGE OverloadedStrings #-}

module Data.Dfa.Format.Html ( asHtml ) where

import           Data.Dfa.Regex
import           Data.FiniteAutomaton (faInputs)
import           Data.Regex

import           Control.Monad        (forM_)
import           Data.Map             as Map
import           Data.Set             as Set
import qualified Data.Text.Lazy       as T
import           Lucid

asHtml :: Bool -> Regex Char -> T.Text
asHtml withoutSkeleton regex =
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
        let regexDfa = fromRegex regex
        thead_ $
            tr_ $ do
                th_ "initial state?"
                th_ "state"
                th_ "final state?"
                forM_ (Set.toAscList . faInputs $ regexDfa) $ -- Ascending order
                    \c ->
                        th_ (toHtml . show $ c)
        tbody_ $ do
            transitionTableRow (regex, transitions regexDfa ! regex)
            -- Leave out the initial regex, as it has been printed before
            forM_ (Map.toList . Map.delete regex . transitions $ regexDfa) transitionTableRow

    transitionTableRow :: (Regex Char, Map Char (Regex Char)) -> Html ()
    transitionTableRow (r, ts) =
        tr_ $ do
            td_ $ if regex == r then "->" else " "
            td_ $ toHtml . show $ r
            td_ $ if matchesEmptyWord r then "*" else " "
            forM_ (Map.elems ts) $ td_ . toHtml . show -- Map.elems yields ascending order
