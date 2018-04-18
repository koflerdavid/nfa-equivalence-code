{- |
Module:      Actions.FiniteAutomataEquivalence
Description: Endpoint for checking the equivalence of two finite automata
Copyright:   (C) David Kofler
License:     BSD3 (see the LICENSE file in the distribution)

Maintainer:  kofler.david@gmail.com
Stability:   provisional
Portability: portable (OverloadedStrings)

This module provides an endpoint for checking the equivalence of two finite automata.

-}
module Actions.FiniteAutomataEquivalence
    ( action
    ) where

import           Control.Monad                ( when )
import           Data.Aeson                   ( ToJSON (toJSON), object, (.=) )
import           Data.Aeson.Text              ( encodeToLazyText )
import           Data.Bifunctor               ( first )
import           Data.Bimap                   ( Bimap, lookup, lookupR )
import           Data.ByteString.Lazy         as LBS
import           Data.IntSet                  ( fromList, toList )
import           Data.Maybe                   ( fromJust )
import           Data.Monoid                  ( (<>) )
import           Data.Text.Encoding           ( decodeUtf8' )
import qualified Data.Text.Lazy               as TL
import           Prelude                      hiding ( lookup )
import           Snap.Core

import           Algorithm.NfaEquivalence
import           Compiler.Hknt
import           Language.Automata.HkntParser

data EquivalenceError
    = ParameterIsMissing
    | Utf8DecodeError
    | ParseError String
    | TranslationError HkntCompileError
    | NoCheckSpecified
    | CheckedStateDoesNotExist String

data PrettyConstraint = PrettyConstraint {
        -- | The input that lead to this constraint
      input     :: String
        -- | The first set of states
    , stateSet1 :: [String]
        -- | The second set of states
    , stateSet2 :: [String]
    }

-- | A trace is the execution sequence of the algorithm
-- Constraints are either ignored or are significant.
data Trace = Trace
    { stateSetChecked    :: Bool
    , stateSetConstraint :: PrettyConstraint
    }

-- | A witness is a string which is accepted by one state set, but not by another
type Witness = PrettyConstraint

data EquivalenceResult
    = Equivalent [Trace]
    | NotEquivalent Witness
                    [Trace]

instance ToJSON EquivalenceResult where
    toJSON (Equivalent trace) = object ["equivalent" .= True, "trace" .= trace]
    toJSON (NotEquivalent witness trace) =
        object ["equivalent" .= False, "witnesses" .= [witness], "trace" .= trace]

instance ToJSON Trace where
    toJSON (Trace checked constraint) = object ["checked" .= checked, "constraint" .= constraint]

instance ToJSON PrettyConstraint where
    toJSON (PrettyConstraint input stateSet1 stateSet2) =
        object ["input" .= input, "stateSet1" .= stateSet1, "stateSet2" .= stateSet2]

{-| This action expects a POST request with content type
`application/x-www-form-urlencoded` and a parameter `input`.
The value of this parameter has to be a UTF-8 string matching the syntax
specified by Bonchi and Pous at the
[web-appendix of their paper](https://perso.ens-lyon.fr/damien.pous/hknt/).
If there is a syntax error, a UTF-8 text message describing the error is
returned.

Only the last `check:` clause in the input will be considered.
Currently, it is always going to be interpreted as an equality check.

After performing the check, a JSON document containing the result
will be encoded and sent to the client.
It will represent an object that always has at least the field `equivalent`.
It is always a boolean stating whether the two states are equivalent.
In case they are not equivalent, an additional field `witnesses`,
containing an array of strings, will be present.
Each of these strings belongs to the language of one of the states,
but not of the other.

-}
action :: Snap ()
action =
    method POST $ do
        input <- fmap LBS.fromStrict <$> getPostParam "input"
        output <- equivalent <$> pure input
        case output of
            Left e -> do
                modifyResponse $ setResponseCode 400
                modifyResponse $ setContentType "text/plain; charset=utf-8"
                case e of
                    ParameterIsMissing -> writeLazyText "The POST parameter \"input\" is missing"
                    Utf8DecodeError -> writeLazyText "The request contains invalid UTF8 codepoints"
                    ParseError syntaxError ->
                        writeLazyText $
                        "The request contains a syntax error:\n" <> TL.pack syntaxError
                    NoCheckSpecified ->
                        writeLazyText "The input does not contain a constraint to check for"
                    CheckedStateDoesNotExist state ->
                        writeLazyText $ "The following state does not exist: " <> TL.pack state
            Right result -> do
                modifyResponse $ setContentType "application/json"
                writeLazyText . encodeToLazyText $ result

equivalent :: Maybe LBS.ByteString -> Either EquivalenceError EquivalenceResult
equivalent Nothing = Left ParameterIsMissing
equivalent (Just utf8Body)
    -- Try to parse UTF8 string
 = do
    body <- first (const Utf8DecodeError) (decodeUtf8' . LBS.toStrict $ utf8Body)
    -- Parse body
    Result transitions acceptingStates checks <- first ParseError $ parseHknt body
    -- Wrap errors into ParseError
    (nfa, stateMapping) <- first TranslationError $ compileHkntToNfa transitions acceptingStates
    -- Ensure there is at least one check
    when (Prelude.null checks) $ Left NoCheckSpecified
    -- Only consider the last constraint. Assume it is an equivalence constraint
    let (stateSet1, _, stateSet2) = Prelude.last checks
    -- Translate the state names to numbers
    stateSet1' <- mapM (translateState stateMapping) stateSet1
    stateSet2' <- mapM (translateState stateMapping) stateSet2
    -- Determine equivalence
    let (maybeWitness, trace) =
            nfaStatesDifferencesHkC
                nfa
                (Data.IntSet.fromList stateSet1')
                (Data.IntSet.fromList stateSet2')
        trace' = Prelude.map (translatedTrace stateMapping) trace
    case maybeWitness of
        Nothing -> return (Equivalent trace')
        Just witness -> return (NotEquivalent (translatedConstraint stateMapping witness) trace')

-- | Translate a set of user input states to the internal states needed for computing equivalence
-- The user could have specified nonexisting states, so we have to watch out for that
translateState :: (Ord s, Ord s', Show s) => Bimap s s' -> s -> Either EquivalenceError s'
translateState stateMapping state =
    maybe
        (Left . CheckedStateDoesNotExist . show $ state)
        return
        (state `lookup` stateMapping)

translatedTrace :: Bimap String Int -> (Bool, Constraint Char) -> Trace
translatedTrace stateMapping (skipped, constraint) =
    Trace
    { stateSetChecked = not skipped
    , stateSetConstraint = translatedConstraint stateMapping constraint
    }

-- | Map back state numbers to the state names specified by the user
-- There will be no check whether the state numbers are valid because they are supposed to come from the compiler.
translatedConstraint :: Bimap String Int -> Constraint Char -> PrettyConstraint
translatedConstraint invStateMapping (input, stateSet1, stateSet2) =
    let prettyStateSet1 =
            Prelude.map (fromJust . (`lookupR` invStateMapping)) . Data.IntSet.toList $
            stateSet1
        prettyStateSet2 =
            Prelude.map (fromJust . (`lookupR` invStateMapping)) . Data.IntSet.toList $
            stateSet2
    in PrettyConstraint input prettyStateSet1 prettyStateSet2
