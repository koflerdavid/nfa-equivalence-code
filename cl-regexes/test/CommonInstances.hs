{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{- |
Module:      CommonInstances
Description: Useful (orphan) instances to make writing tests easier
Copyright:   (C) David Kofler
License:     BSD3 (see the LICENSE file in the distribution)

Maintainer:  kofler.david@gmail.com
Stability:   provisional
Portability: portable (Haskell 2010)
-}
module CommonInstances where

import Data.Regex           ( Regex(..) )
import Language.RegexParser ( parseRegex )

import Data.String          ( IsString (..) )
import Data.Text            as T ( pack )
import Test.QuickCheck

instance IsString (Regex Char) where
    fromString regexString =
        case parseRegex "haskell source code" (T.pack regexString) of
            Left e -> error $ "Could not compile " ++ show regexString ++ " : " ++ e
            Right regex -> regex

instance Arbitrary (Regex Char) where
    arbitrary = sized regex
      where
        alphabeticCharacters = ['A'..'Z'] ++ ['a'..'z']
        regex n
            | 0 == n = oneof [Atom <$> elements alphabeticCharacters, pure Epsilon, pure Empty]
            | otherwise =
                oneof
                    [ KleeneStar <$> smallerThan n
                    , KleenePlus <$> smallerThan n
                    , Sequence <$> smallerThan n <*> smallerThan n
                    , Alternative <$> smallerThan n <*> smallerThan n
                    ]
        smallerThan n = regex (n `div` 2)
