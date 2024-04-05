module RandomWalk (walk, genMoves) where

import System.Random
import WeightedRandom

walk :: Num a => (a, a) -> (a, a) -> (a, a)
walk (x, y) (dx, dy) = (x + dx, y + dy)

genMoves :: (RandomGen g, Num a) => g -> [(Int, a)] -> [(Int, a)] -> [(a, a)]
genMoves gen xws yws = 
    let (x, gen')  = weighted gen xws
        (y, gen'') = weighted gen' yws
    in (x, y) : genMoves gen'' xws yws