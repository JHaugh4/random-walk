module GlossMain (run) where

import Graphics.Gloss
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.ViewPort

import Data.List

import System.Random

import qualified RandomWalk as RW
import qualified WeightedRandom as WR

addPoints :: Point -> Point -> Point
addPoints (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

type Model = ([Path], [Point])

updatePaths :: Model -> Model
updatePaths (paths, moves) = foldl go ([], moves) paths
    where
        go (paths, m:ms) (point:path) = ((addPoints point m : point : path) : paths, ms)

modelToPicture :: Model -> Picture
modelToPicture (paths, _) = 
    pictures $ map (color green . line) paths

step :: ViewPort -> Float -> Model -> Model
step vp s = updatePaths

xWeights, yWeights :: Num a => [(Int, a)]
xWeights = [(5, -1), (90, 0), (5, 1)]
yWeights = [(10, 0), (90, 1)]

run :: IO ()
run = do
    putStrLn "How big would you like the window?"
    wSizeStr <- getLine
    let wSize = read wSizeStr
    let window = InWindow "Random Walk" (wSize, wSize) (0, 0)
    gen <- initStdGen
    let moves = RW.genMoves gen xWeights yWeights
    let initialPath = [[(0, -(fromIntegral wSize / 2))]]
    simulate window black 15 (initialPath, moves) modelToPicture step