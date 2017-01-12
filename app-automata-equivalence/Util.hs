module Util where

import           Data.Map as Map

invertMap :: (Ord t) => Map s t -> Map t s
invertMap m = Map.fromList [ (s, name)
                           | (name, s) <- Map.toList m ]
