module RandomNfa ( randomNfa ) where

import           Data.Nfa

import           Control.Monad        ( forM )
import           Control.Monad.Random
import           Control.Monad.Random.Class
--import           Control.Monad.Random.Class
import qualified Data.List            as List
import qualified Data.Map             as Map
import qualified Data.IntSet          as IntSet

-- | This algorithm is essentially a rewrite of Bonchi and Pous' code to generate random NFAs
randomNfa :: (Monad m, Ord c, RandomGen g) => Int -> [c] -> Float -> Float -> RandT g m (Nfa c)
randomNfa n cs transitionDensity acceptanceProbability =
    let p = transitionDensity / (fromIntegral n)
        stateInputPairs = [ (q, c)
                          | q <- [0 .. n]
                          , c <- List.sort cs ]
        transitions = forM stateInputPairs $
            \q_c -> do
                states <- pickStates p
                return (q_c, IntSet.fromList states)
        transitionMap = Map.fromList <$> transitions
    in
        Nfa <$> (IntSet.fromList <$> pickStates acceptanceProbability) <*> transitionMap
  where
    pickStates p = randomSet [0 .. n] p

-- | Selects a random subset of the given list. Each element is chosen
randomSet :: (Monad m, RandomGen g) => [a] -> Float -> RandT g m [a]
randomSet source p = do
    randomNumbers <- getRandomRs (0, 1)
    let pairs = zip source randomNumbers
        samples = filter ((< p) . snd) pairs
    return . map fst $ samples
