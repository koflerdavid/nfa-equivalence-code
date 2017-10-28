{-# LANGUAGE OverloadedStrings #-}

module RegexHandlers where

import Algorithm.Regex.Derivation
import Data.List as List
import Data.Map as Map
import qualified Data.Queue as Queue
import Data.Regex
import Data.Set as Set
import Language.RegexParser
import RegexDfaOutput.Html

import           Control.Monad.Trans.State  ( State, evalState, gets, modify )
import Data.ByteString.Char8 (unpack)
import Data.Maybe (isNothing)
import Data.String (fromString)
import Snap.Core

type RegexDfaTransitions = Map (Regex Char) (Map Char (Regex Char))

derivationHandler :: Snap ()
derivationHandler =
  methods [POST] $ do
    setTimeout 10
    modifyResponse $ setContentType "text/plain; charset=utf-8"
    mInputRegexString <- getParam "regex"
    mWord <- getParam "word"
    case (mInputRegexString, mWord) of
      (Nothing, _) -> writeBS "No regex given"
      (_, Nothing) -> writeBS "No word given"
      (Just inputRegexString, Just word) ->
        case parseRegex "<param>" (unpack inputRegexString) of
          Left _parseError -> writeBS "parse error"
          Right regex -> do
            let regex' = wordDerive (unpack word) regex
            writeBS $ fromString (show regex')

regexToDfaConversionHandler :: Snap ()
regexToDfaConversionHandler =
  methods [POST] $ do
    setTimeout 10
    modifyResponse $ setContentType "text/html; charset=utf-8"
    mRegexString <- getParam "regex"
    case mRegexString of
      Nothing -> writeBS "No regex given"
      Just regexString ->
        case parseRegex "<param>" (unpack regexString) of
          Left _parseError -> writeBS "Regex parse error"
          Right regex -> do
            let transitions = deriveRegexToDfa regex
            withHeader <- fmap (isNothing . getHeader "X-Embeddable") getRequest
            writeLazyText (formatTransitionTable withHeader regex transitions)

deriveRegexToDfa :: Regex Char -> RegexDfaTransitions
deriveRegexToDfa regex =
    let queue = Queue.empty `Queue.push` regex
    in
        evalState processQueue (Map.empty, queue)
  where
    regexAlphabet = Set.toAscList (alphabet regex)
    processQueue :: State (RegexDfaTransitions, Queue.FifoQueue (Regex Char)) RegexDfaTransitions
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
deriveRegexForAlphabet :: Queue.Queue q
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
