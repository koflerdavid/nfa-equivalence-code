{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}
module Main where

import DataFiles                         ( getDataFileName )

import Actions.Derivation                as Derivation
import Actions.FiniteAutomataEquivalence as FiniteAutomataEquivalence
import Actions.RegexEquivalence          as RegexEquivalence
import Actions.RegexToDfaConversion      as RegexToDfaConversion

import Control.Applicative               ( (<|>) )
import Control.Monad.IO.Class            ( liftIO )
import Data.ByteString
import Snap.Core
import Snap.Http.Server
import Snap.Util.FileServe

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site =
    ifTop serveIndex
    <|> route
        [ ("finiteAutomata/equivalence", FiniteAutomataEquivalence.action)
        , ("regex/derivation", Derivation.action)
        , ("regex/dfa_conversion", RegexToDfaConversion.action)
        , ("regex/equivalence", RegexEquivalence.action)
        ]
    <|> serveStatic "static" "static"
    <|> notFoundError

serveIndex :: Snap ()
serveIndex = do
    modifyResponse $ setContentType "text/html; charset=utf-8"
    sendFile =<< liftIO (getDataFileName "static/index.html")

serveStatic :: ByteString -> FilePath -> Snap ()
serveStatic pathComponent baseDirectory = do
    dir pathComponent $ do
        dataFileBaseDir <- liftIO (getDataFileName baseDirectory)
        serveDirectory dataFileBaseDir

notFoundError :: Snap ()
notFoundError = do
    modifyResponse $ setContentType "text/plain; charset=utf-8"
    modifyResponse $ setResponseCode 404
    writeLazyText "404 Not Found"
