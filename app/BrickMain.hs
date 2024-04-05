module BrickMain where

import Brick.Main (App(..), customMain, neverShowCursor, resizeOrQuit)
import Brick.Types (Widget, BrickEvent(..), EventM, get, put, Location(..))
import Brick.Widgets.Core (str, withAttr, translateBy)
import Brick.Widgets.Table (table, renderTable)
import Brick.BChan (newBChan, writeBChan)
import Brick.AttrMap (AttrMap, attrMap, AttrName, attrName)
import Brick.Util (on)

import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform as VCP

import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (void, forever)

import System.Random

import qualified RandomWalk as RW

import qualified Debug.Trace as DT 

type MyState = ([Location], [Location])

pointAttr :: AttrName
pointAttr = attrName "pointAttr"

drawPoint :: Location -> Widget ()
drawPoint loc = 
    translateBy loc $ withAttr pointAttr (str " ")

drawState :: MyState -> [Widget ()]
drawState = map drawPoint . fst

data ClockTick = Tick

handleEvent :: BrickEvent () ClockTick -> EventM () MyState ()
handleEvent (AppEvent Tick) = do
    (locs, moves) <- get
    put (head locs <> head moves : locs, tail moves)
-- Any other event just do nothing
handleEvent bevent = resizeOrQuit bevent

theMap :: AttrMap
theMap = attrMap V.defAttr
    [ (pointAttr, V.green `on` V.green) ]

app :: App MyState ClockTick ()
app = App
  { appDraw         = drawState
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = handleEvent
  , appStartEvent   = pure ()
  , appAttrMap      = const theMap
  }

genRandomMoves :: (RandomGen g) => g -> [Location]
genRandomMoves gen =
    let (x, gen') = uniformR (-1, 1) gen
        (y, gen'') = uniformR (-1, 1) gen'
    in Location (x, y) : genRandomMoves gen''

run :: IO ()
run = do
    let delay = 100000 -- 100 ms
    chan <- newBChan 10
    void . forkIO $ forever $ do
        writeBChan chan Tick
        threadDelay delay
    --
    let builder = VCP.mkVty V.defaultConfig
    initialVty <- builder
    --
    let initLoc = Location (100, 50)
    gen <- initStdGen
    let moves = genRandomMoves gen
    -- Zoom out after starting
    customMain initialVty builder (Just chan) app ([initLoc], moves)
    return ()