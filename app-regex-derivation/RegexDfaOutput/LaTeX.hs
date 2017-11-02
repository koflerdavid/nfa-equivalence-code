{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module RegexDfaOutput.LaTeX ( printTransitionTable ) where

import Data.Dfa.Regex               ( RegexDfaTransitions )
import Data.Regex

import Data.List                    as List
import Data.Map                     as Map
import Data.Set                     as Set
import Text.LaTeX
import Text.LaTeX.Base.Class
import Text.LaTeX.Base.Pretty
import Text.LaTeX.Packages.AMSMath
import Text.LaTeX.Packages.AMSSymb
import Text.LaTeX.Packages.Inputenc

printTransitionTable :: Bool -> Regex Char -> RegexDfaTransitions -> IO ()
printTransitionTable withoutSkeleton regex transitions = do
    let regexAlphabet = alphabet regex
        firstColumn = [ LeftColumn, DVerticalLine ]
        stateColumns = take (2 * Set.size regexAlphabet) $ cycle [ LeftColumn, VerticalLine ]
        table = tabular (Just Center)
                        (firstColumn ++ stateColumns)
                        (tableHeader regexAlphabet <> lnbk <> hline <> tableBody transitions)
        theDocument = if withoutSkeleton then table else thePreamble <> document table
    putStrLn . prettyLaTeX $ theDocument

thePreamble :: LaTeX
thePreamble = mconcat [ documentclass [] article
                      , usepackage [ utf8 ] inputenc
                      , usepackage [] amsmath
                      , usepackage [] amssymb
                      ]

tableHeader :: Set Char -> LaTeX
tableHeader inputs = do
    List.foldl1 (&) $ "state" : (List.map stateName . Set.toAscList $ inputs)

stateName :: Char -> LaTeX
stateName s = fromString (show s)

regexRepr :: Regex Char -> LaTeX
regexRepr = texy . TexyRegex

tableBody :: RegexDfaTransitions -> LaTeX
tableBody transitions = mconcat $ List.map (uncurry stateTransitions) (Map.toList transitions)

stateTransitions :: Regex Char -> Map Char (Regex Char) -> LaTeX
stateTransitions r ts = columns
  where
    maybeStar = if matchesEmptyWord r then math (raw "\\star") else mempty
    theTitle = maybeStar <> regexRepr r
    columns = List.foldl1 (&) (theTitle : (List.map regexRepr . Map.elems $ ts)) <> lnbk

newtype TexyRegex c = TexyRegex { innerRegex :: Regex c }

instance Texy (TexyRegex Char) where
    texy r = math $ texyPrec 0 $ innerRegex r

texyPrec :: (LaTeXC l, Show c) => Int -> Regex c -> l
texyPrec _ Empty = raw "\\varnothing"
texyPrec _ Epsilon = epsilon
texyPrec _ (Atom c) = fromString (show c)
texyPrec prec (Alternative r s) =
    let inner = texyPrec 0 r <> raw "+" <> texyPrec 0 s
    in
        if prec > 0 then autoParens inner else inner
texyPrec prec (Sequence r s) =
    let inner = texyPrec 1 r <> quad <> texyPrec 1 s
    in
        if prec > 1 then autoParens inner else inner
texyPrec _ (Asterisk r) =
    texyPrec 2 r ^: raw "\\ast"
