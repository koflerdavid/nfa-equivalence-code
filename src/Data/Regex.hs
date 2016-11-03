{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.Regex
  (
    Regex(..)
  , alphabet
  ) where

import Control.Monad.Trans.State.Lazy
import Data.Set as S

data Regex c = Atom c
             | Optional (Regex c)
             | Asterisk (Regex c)
             | forall identifier. (Ord identifier, Show identifier) => Capture (Regex c) (Maybe identifier)
             | Alternative [Regex c]
             | Sequence [Regex c]

deriving instance Show c => Show (Regex c)

alphabet :: Ord c => Regex c -> S.Set c
alphabet regex = execState (computeAlphabet regex) (S.empty)
  where computeAlphabet r = case r of
          Atom c -> modify (S.insert c)
          Optional inner -> computeAlphabet inner
          Asterisk inner -> computeAlphabet inner
          Capture inner _id -> computeAlphabet inner
          Alternative rs -> mapM_ computeAlphabet rs
          Sequence rs -> mapM_ computeAlphabet rs
