module Main where

import Actions.Derivation                as Derivation
import Actions.FiniteAutomataEquivalence as FiniteAutomataEquivalence
import Actions.RegexEquivalence          as RegexEquivalence
import Actions.RegexToDfaConversion      as RegexToDfaConversion

import Control.Applicative               ( (<|>) )
import Snap.Core
import Snap.Http.Server
import Snap.Util.FileServe

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site =
    ifTop (modifyResponse (setContentType "text/html; charset=utf-8") >> sendFile "static/index.html") <|>
    route
        [ ("finiteAutomata/equivalence", FiniteAutomataEquivalence.action)
        , ("regex/derivation", Derivation.action)
        , ("regex/dfa_conversion", RegexToDfaConversion.action)
        , ("regex/equivalence", RegexEquivalence.action)
        ] <|>
    dir "static" (serveDirectory "static") <|>
    notFoundError

notFoundError :: Snap ()
notFoundError = do
    modifyResponse $ setContentType "text/plain; charset=utf-8"
    modifyResponse $ setResponseCode 404
    writeLazyText "404 Not Found"
