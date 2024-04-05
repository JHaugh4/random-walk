module WeightedRandom (weighted) where

import System.Random

weighted :: (RandomGen g) => g -> [(Int, b)] -> (b, g)
weighted gen was = (as !! x, g')
    where
        weightSum = sum $ map fst was
        as = concatMap (uncurry replicate) was
        (x, g') = uniformR (0, weightSum - 1) gen