module Main where

import qualified GlossMain as GM
import qualified BrickMain as BM

main :: IO ()
main = do
    putStrLn "gloss or brick?"
    ans <- getLine
    case ans of
        "gloss" -> GM.run
        "brick" -> BM.run

