module Actions.FiniteAutomataEquivalence
    ( action
    ) where

import           Control.Monad                ( when )
import           Data.Aeson                   ( ToJSON (toJSON), object, (.=) )
import           Data.Aeson.Text              ( encodeToLazyText )
import           Data.ByteString.Lazy         as LBS
import           Data.ByteString.UTF8         as UTF8
import           Data.Either.Combinators      ( mapLeft )
import           Data.IntSet                  ( fromList, toList )
import           Data.Bimap                   ( Bimap, lookup, lookupR )
import           Data.Maybe                   ( fromJust )
import           Data.Monoid                  ( (<>) )
import qualified Data.Text.Lazy               as T
import           Prelude                      hiding ( lookup )
import           Snap.Core

import           Algorithm.NfaEquivalence
import           Compiler.Hknt
import           Language.Automata.HkntParser

data EquivalenceError
    = ParameterIsMissing
    | Utf8DecodeError
    | ParseError String
    | NoCheckSpecified
    | CheckedStateDoesNotExist String

data PrettyConstraint =
    PrettyConstraint String -- | The input that lead to this constraint
                     [String] -- | The first set of states
                     [String] -- | The second set of states

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
                        "The request contains a syntax error:\n" <> T.pack syntaxError
                    NoCheckSpecified ->
                        writeLazyText "The input does not contain a constraint to check for"
                    CheckedStateDoesNotExist state ->
                        writeLazyText $ "The following state does not exist: " <> T.pack state
            Right result -> do
                modifyResponse $ setContentType "application/json"
                writeLazyText . encodeToLazyText $ result

equivalent :: Maybe LBS.ByteString -> Either EquivalenceError EquivalenceResult
equivalent Nothing = Left ParameterIsMissing
equivalent (Just utf8Body)
    -- Try to parse UTF8 string
 = do
    body <- maybe (Left Utf8DecodeError) Right (fromUtf8 . LBS.toStrict $ utf8Body)
    -- Parse body
    Result transitions acceptingStates checks <- mapLeft ParseError $ parseHknt body
    -- Wrap errors into ParseError
    (nfa, stateMapping) <- mapLeft ParseError $ compileHkntToNfa transitions acceptingStates
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

fromUtf8 :: UTF8.ByteString -> Maybe String
fromUtf8 bs = do
    let str = UTF8.toString bs
    if UTF8.replacement_char `Prelude.elem` str
        then Nothing
        else Just str

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
