{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module RegexFullDerivation where

import           Algorithm.Regex.Derivation
import           Data.Queue                  as Queue
import           Data.Regex
import           Language.RegexParser

import           Control.Monad
import           Control.Monad.Trans.Class   ( lift )
import           Control.Monad.Trans.Except  ( ExceptT, runExceptT, throwE )
import           Control.Monad.Trans.State
import           Data.List                   as List
import           Data.Map                    as Map
import           Data.Set                    as Set
import qualified Data.Text                   as T
import qualified Data.Text.IO                as TIO
import           System.IO
import           Text.LaTeX
import           Text.LaTeX.Base.Texy
import           Text.LaTeX.Base.Class
import           Text.LaTeX.Packages.AMSMath
import           Text.LaTeX.Packages.AMSSymb

parseAndDeriveRegexToDfa :: ExceptT String IO ()
parseAndDeriveRegexToDfa = do
    input <- lift $ getContents
    case parseRegex "<stdin>" input of
        Left parseError -> throwE parseError
        Right regex -> do
            lift $ printTransitionTable regex (deriveRegexToDfa regex)

type RegexDfaTransitions = Map.Map (Regex Char) (Map.Map Char (Regex Char))

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

printTransitionTable :: Regex Char -> RegexDfaTransitions -> IO ()
printTransitionTable regex transitions = do
    let regexAlphabet = alphabet regex
        firstColumn = [ LeftColumn, DVerticalLine ]
        stateColumns = take (2 * Set.size regexAlphabet) $ cycle [ LeftColumn, VerticalLine ]
        table = tabular (Just Center)
                        (firstColumn ++ stateColumns)
                        (tableHeader regexAlphabet <> lnbk <> hline <> tableBody transitions)
    TIO.putStrLn . render $ table
  where
    tableHeader :: Set Char -> LaTeX
    tableHeader inputs = do
        List.foldl1 (&) $ "state" : (List.map stateName . Set.toAscList $ inputs)

    stateName :: Char -> LaTeX
    stateName s = fromString (show s)

    regexRepr :: Regex Char -> LaTeX
    regexRepr = texy

    tableBody :: RegexDfaTransitions -> LaTeX
    tableBody transitions = mconcat $ List.map (uncurry stateTransitions) (Map.toList transitions)

    stateTransitions :: Regex Char -> Map Char (Regex Char) -> LaTeX
    stateTransitions r ts = columns
      where
        maybeStar = if matchesEmptyWord r then math (raw "\\star") else mempty
        title = maybeStar <> regexRepr r
        columns = List.foldl1 (&) (title : (List.map regexRepr . Map.elems $ ts)) <> lnbk

instance Texy (Regex Char) where
    texy r = math $ texyPrec 0 r

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
