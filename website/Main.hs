module Main where

import Actions.Derivation           as Derivation
import Actions.RegexToDfaConversion as RegexToDfaConversion

import Control.Applicative          ( (<|>) )
import Snap.Core
import Snap.Http.Server
import Snap.Util.FileServe

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site =
    ifTop (sendFile "website/static/index.html") <|>
    route
        [ ("regex/derivation", Derivation.action)
        , ("regex/dfa_conversion", RegexToDfaConversion.action)
        ] <|>
    dir "static" (serveDirectory "website/static")
