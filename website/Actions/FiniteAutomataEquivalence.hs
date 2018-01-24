module Actions.FiniteAutomataEquivalence
    ( action
    ) where

import           Control.Monad                ( when )
import           Data.Aeson                   ( ToJSON (toJSON), object, (.=) )
import           Data.Aeson.Text              ( encodeToLazyText )
import           Data.ByteString.Lazy         as LBS
import           Data.ByteString.UTF8         as UTF8
import           Data.Either.Combinators      ( mapLeft )
import           Data.IntSet                  ( fromList )
import           Data.Map                     ( Map, lookup )
import           Data.Monoid                  ( (<>) )
import qualified Data.Text.Lazy               as T
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

data EquivalenceResult
    = Equivalent
    | NotEquivalent [String]

instance ToJSON EquivalenceResult where
    toJSON Equivalent = object ["equivalent" .= True]
    toJSON (NotEquivalent witnesses) =
        object ["equivalent" .= False, "witnesses" .= witnesses]

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
    Result transitions acceptingStates checks <-
        mapLeft ParseError $ parseHknt body
    -- Wrap errors into ParseError
    (nfa, stateMapping) <-
        mapLeft ParseError $ compileHkntToNfa transitions acceptingStates
    -- Ensure there is at least one check
    when (Prelude.null checks) $ Left NoCheckSpecified
    -- Only consider the last constraint. Assume it is an equivalence constraint
    let (stateSet1, _, stateSet2) = Prelude.last checks
    -- Translate the state names to numbers
    stateSet1' <- mapM (translateState stateMapping) stateSet1
    stateSet2' <- mapM (translateState stateMapping) stateSet2
    -- Determine equivalence
    let (maybeWitness, _) =
            nfaStatesDifferencesHkC
                nfa
                (Data.IntSet.fromList stateSet1')
                (Data.IntSet.fromList stateSet2')
    case maybeWitness of
        Nothing              -> return Equivalent
        Just (witness, _, _) -> return (NotEquivalent [witness])

fromUtf8 :: UTF8.ByteString -> Maybe String
fromUtf8 bs = do
    let str = UTF8.toString bs
    if UTF8.replacement_char `Prelude.elem` str
        then Nothing
        else Just str

translateState :: (Ord s, Show s) => Map s s' -> s -> Either EquivalenceError s'
translateState stateMapping state =
    maybe
        (Left . CheckedStateDoesNotExist . show $ state)
        return
        (state `Data.Map.lookup` stateMapping)
