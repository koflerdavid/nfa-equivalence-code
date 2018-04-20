{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module RegexDfaOutput.LaTeX
    ( printTransitionTable
    ) where

import Algorithm.Regex.DfaConversion ( RegexDfaTransitions, fromRegex,
                                       regexDfaTransitions )
import Data.Regex

import Data.List                     as List
import Data.Map                      as Map
import Data.Set                      as Set
import Text.LaTeX                    ( LaTeX, Pos (..), TableSpec (..),
                                       Texy (..), article, document,
                                       documentclass, fromString, hline, lnbk,
                                       raw, tabular, usepackage, (&), (<>) )
import Text.LaTeX.Base.Class
import Text.LaTeX.Base.Pretty
import Text.LaTeX.Packages.AMSMath
import Text.LaTeX.Packages.AMSSymb
import Text.LaTeX.Packages.Inputenc

printTransitionTable :: Bool -> Regex Char -> IO ()
printTransitionTable withoutSkeleton regex = do
    let regexAlphabet = alphabet regex
        firstColumn = [LeftColumn, DVerticalLine]
        stateColumns = repeatList (Set.size regexAlphabet) [LeftColumn, VerticalLine]
        table =
            tabular
                (Just Center)
                (firstColumn ++ stateColumns)
                (tableHeader regexAlphabet <> lnbk <> hline <> ((tableBody . regexDfaTransitions) (fromRegex regex)))
        theDocument =
            if withoutSkeleton
                then table
                else thePreamble <> document table
    putStrLn . prettyLaTeX $ theDocument

thePreamble :: LaTeX
thePreamble =
    mconcat
        [ documentclass [] article
        , usepackage [utf8] inputenc
        , usepackage [] amsmath
        , usepackage [] amssymb
        ]

tableHeader :: Set Char -> LaTeX
tableHeader inputs = List.foldl1 (&) $ "state" : (List.map stateName . Set.toAscList $ inputs)

stateName :: Char -> LaTeX
stateName s = fromString (show s)

regexRepr :: Regex Char -> LaTeX
regexRepr = texy . TexyRegex

tableBody :: RegexDfaTransitions Char -> LaTeX
tableBody ts = mconcat $ List.map (uncurry stateTransitions) (Map.toList ts)

stateTransitions :: Regex Char -> Map Char (Regex Char) -> LaTeX
stateTransitions r ts = columns
  where
    maybeStar =
        if matchesEmptyWord r
            then math (raw "\\star")
            else mempty
    theTitle = maybeStar <> regexRepr r
    columns = List.foldl1 (&) (theTitle : (List.map regexRepr . Map.elems $ ts)) <> lnbk

repeatList :: Int -> [a] -> [a]
repeatList n = List.concat . List.replicate n

newtype TexyRegex c = TexyRegex { innerRegex :: Regex c }

instance Texy (TexyRegex Char) where
    texy r = math $ texyPrec 0 $ innerRegex r

texyPrec :: (LaTeXC l, Show c) => Int -> Regex c -> l
texyPrec _ Empty = raw "\\varnothing"
texyPrec _ Epsilon = epsilon
texyPrec _ (Atom c) = fromString (show c)
texyPrec prec (Alternative r s) =
    autoParensWhen (prec > 0) $ texyPrec 0 r <> raw "+" <> texyPrec 0 s
texyPrec prec (Sequence r s) = autoParensWhen (prec > 1) $ texyPrec 1 r <> quad <> texyPrec 1 s
texyPrec _ (KleeneStar r) = texyPrec 2 r ^: raw "\\ast"
texyPrec _ (KleenePlus r) = texyPrec 2 r ^: raw "\\dagger"

autoParensWhen :: LaTeXC l => Bool -> l -> l
autoParensWhen True  = autoParens
autoParensWhen False = id
