module RandomNfa ( randomNfa ) where

import           Data.Nfa

import           Control.Monad        ( forM )
import           Control.Monad.Random
import qualified Data.List            as List
import qualified Data.Map             as Map
import qualified Data.IntSet          as IntSet

-- | This algorithm is essentially a rewrite of Bonchi and Pous' code to generate random NFAs
randomNfa :: (Monad m, Ord c, RandomGen g) => Int -> [c] -> Float -> Float -> RandT g m (Nfa c)
randomNfa n cs transitionDensity acceptanceProbability = do
    let p = transitionDensity / (fromIntegral n)
        stateInputPairs = [ (q, c)
                          | q <- [0 .. n] 
                          , c <- List.sort cs ]
    transitions <- forM stateInputPairs $
                       \q_c -> do
                           states <- pickStates p
                           return (q_c, IntSet.fromList states)
    let transitionMap = Map.fromList transitions
    acceptingStates <- pickStates acceptanceProbability
    let nfa = Nfa (IntSet.fromList acceptingStates) transitionMap
    return (prunedNfa nfa)
  where
    pickStates p = randomSet [0 .. n] p

-- | Selects a random subset of the given list. Each element is chosen
randomSet :: (Monad m, RandomGen g) => [a] -> Float -> RandT g m [a]
randomSet source p = do
    randomNumbers <- getRandomRs (0, 1)
    let pairs = zip source randomNumbers
        samples = filter ((< p) . snd) pairs
    return . map fst $ samples

prunedNfa :: Ord c => Nfa c -> Nfa c
prunedNfa nfa = let transitions = Map.toList (nfaTransitions nfa)
                    reachableStates = getReachableStates [ 0 ] transitions
                    reachableTransitions = filter (\((p, _), _) -> p `IntSet.member` reachableStates)
                                                  transitions
                in
                    nfa { nfaTransitions = Map.fromList reachableTransitions
                        , nfaAcceptingStates = nfaAcceptingStates nfa `IntSet.intersection`
                            reachableStates
                        }

getReachableStates :: [Int] -> [((Int, c), IntSet.IntSet)] -> IntSet.IntSet
getReachableStates initialStates transitions =
    acc (IntSet.fromList initialStates) transitions False []
  where
    acc states currentTransitions retry toRetry =
        case currentTransitions of
            [] -> if retry then acc states toRetry False [] else states
            t@((p, _), qs) : ts
                | p `IntSet.member` states ->
                      acc (qs `IntSet.union` states) ts True toRetry
                | otherwise -> acc states ts retry (t : toRetry)
