module Levels where


import Brick
import Brick.BChan
import qualified Graphics.Vty as V

import Control.Monad --(forever, void, transformer)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay, forkIO)
import Data.Maybe (fromMaybe)
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Border as B


data Game = Game
    { _explorer :: Coord
    , _mummies  :: [Mummy]
    , _trap :: [Coord]
    , _keys :: [Coord]
    , _vwalls    :: [Coord]
    , _hwalls    :: [Coord]
    , _bsize    :: Int
    , _exit     :: Coord
    , _lock    :: Bool
    , _gameState :: GameState
    , _level :: Int
    , _keyCount :: Int
    }
-- Level 0 = select screen
-- Level -1 = Lose
-- Level -2 = Win

data Coord = Coord { x :: Int, y :: Int} deriving (Eq, Ord, Show)

data GameState = SelectScreen | Win | Lose | Playing | Over deriving (Show, Eq)

data Mummy = Mummy
    { _mloc :: Coord
    , _mct :: Int
    , _mrct :: Int
    }


test :: Game
test = Game { _explorer = Coord 1 1
                 , _mummies  = [(Mummy {_mloc = Coord 6 6, _mct = 2, _mrct = 0}), (Mummy {_mloc = Coord 8 3, _mct = 2, _mrct = 0})]
                 , _trap = [Coord 2 2]
                 , _keys = [Coord 2 1]
                 , _vwalls    = [Coord 2 3, Coord 2 4, Coord 6 6, Coord 8 2, Coord 8 3, Coord 8 4]
                 , _hwalls    = [Coord 3 0, Coord 2 3, Coord 3 3, Coord 6 6, Coord 7 6]
                 , _bsize    = 10
                 , _exit     = Coord 3 0
                 , _lock     = True
                 , _gameState = Playing
                 , _level = 1
                 , _keyCount = 1
                 }
level_1 :: Game
level_1 = Game { _explorer = Coord 5 5
                 , _mummies  = [(Mummy {_mloc = Coord 8 3, _mct = 2, _mrct = 0})]
                 , _trap = []
                 , _keys = []
                 , _vwalls    = [Coord 8 2, Coord 8 3, Coord 8 4]
                 , _hwalls    = [Coord 2 3,Coord 3 0, Coord 3 3, Coord 6 6, Coord 7 6]
                 , _bsize    = 10
                 , _exit     = Coord 3 0
                 , _lock     = True
                 , _gameState = Playing
                 , _level = 1
                 , _keyCount = 0
                 } 

level_2 :: Game
level_2 = Game { _explorer = Coord 2 4
                 , _mummies  = [(Mummy {_mloc = Coord 0 9, _mct = 2, _mrct = 0})]
                 , _trap = []
                 , _keys = []
                 , _vwalls    = []
                 , _hwalls    = [Coord 1 0]
                 , _bsize    = 10
                 , _exit     = Coord 1 0
                 , _lock     = True
                 , _gameState = Playing
                 , _level = 2
                 , _keyCount = 0
                 }
level_3 :: Game
level_3 = Game { _explorer = Coord 2 1
                 , _mummies  = [(Mummy {_mloc = Coord 8 5, _mct = 2, _mrct = 0}), (Mummy {_mloc = Coord 8 3, _mct = 2, _mrct = 0})]
                 , _trap = [Coord 2 2 , Coord 4 4]
                 , _keys = [Coord 4 2]
                 , _vwalls    = [Coord 0 7, Coord 2 2, Coord 3 2,Coord 5 1, Coord 5 4, Coord 5 5, Coord 6 1, Coord 6 2, Coord 6 3, Coord 6 4]
                 , _hwalls    = [Coord 0 1, Coord 1 1,Coord 2 1, Coord 6 1, Coord 5 5, Coord 7 6, Coord 8 6]
                 , _bsize    = 10
                 , _exit     = Coord 0 7
                 , _lock     = True
                 , _gameState = Playing
                 , _level = 3
                 , _keyCount = 1
                 }
level_4 :: Game
level_4 = Game {_explorer = Coord 1 1
                 , _mummies  = [(Mummy {_mloc = Coord 6 6, _mct = 2, _mrct = 0}), (Mummy {_mloc = Coord 8 3, _mct = 2, _mrct = 0})]
                 , _trap = [Coord 2 2]
                 , _keys = [Coord 2 1]
                 , _vwalls    = [Coord 2 3, Coord 2 4, Coord 6 6, Coord 8 2, Coord 8 3, Coord 8 4]
                 , _hwalls    = [Coord 2 3,Coord 3 0, Coord 3 3, Coord 6 6, Coord 7 6]
                 , _bsize    = 10
                 , _exit     = Coord 3 0
                 , _lock     = True
                 , _gameState = Playing
                 , _level = 4
                 , _keyCount = 1
                 }   
level_5 :: Game
level_5 = Game { _explorer = Coord 3 2
                 , _mummies  = [(Mummy {_mloc = Coord 4 0, _mct = 2, _mrct = 0})]
                 , _trap = [Coord 1 5]
                 , _keys = [Coord 4 5]
                 , _vwalls    = [Coord 2 1, Coord 3 0, Coord 0 3, Coord 1 4, Coord 3 5]
                 , _hwalls    = [Coord 4 2, Coord 3 2, Coord 5 4,Coord 5 1,Coord 1 5, Coord 7 6]
                 , _bsize    = 10
                 , _exit     = Coord 0 3
                 , _lock     = True
                 , _gameState = Playing
                 , _level = 5
                 , _keyCount = 1
                 } 
level_6 :: Game
level_6 = Game { _explorer = Coord 1 1
                 , _mummies  = [(Mummy {_mloc = Coord 6 6, _mct = 2, _mrct = 0}), (Mummy {_mloc = Coord 8 3, _mct = 2, _mrct = 0})]
                 , _trap = [Coord 2 2]
                 , _keys = [Coord 2 1]
                 , _vwalls    = [Coord 2 3, Coord 2 4, Coord 6 6, Coord 8 2, Coord 8 3, Coord 8 4]
                 , _hwalls    = [Coord 3 0, Coord 2 3, Coord 3 3, Coord 6 6, Coord 7 6]
                 , _bsize    = 10
                 , _exit     = Coord 3 0
                 , _lock     = True
                 , _gameState = Playing
                 , _level = 6
                 , _keyCount = 1
                 }
level_7 :: Game
level_7 = Game { _explorer = Coord 1 1
                 , _mummies  = [(Mummy {_mloc = Coord 6 6, _mct = 2, _mrct = 0}), (Mummy {_mloc = Coord 8 3, _mct = 2, _mrct = 0})]
                 , _trap = [Coord 2 2]
                 , _keys = [Coord 2 1]
                 , _vwalls    = [Coord 2 3, Coord 2 4, Coord 6 6, Coord 8 2, Coord 8 3, Coord 8 4]
                 , _hwalls    = [Coord 3 0, Coord 2 3, Coord 3 3, Coord 6 6, Coord 7 6]
                 , _bsize    = 10
                 , _exit     = Coord 3 0
                 , _lock     = True
                 , _gameState = Playing
                 , _level = 7
                 , _keyCount = 1
                 }
level_8 :: Game
level_8 = Game { _explorer = Coord 1 1
                 , _mummies  = [(Mummy {_mloc = Coord 6 6, _mct = 2, _mrct = 0}), (Mummy {_mloc = Coord 8 3, _mct = 2, _mrct = 0})]
                 , _trap = [Coord 2 2]
                 , _keys = [Coord 2 1]
                 , _vwalls    = [Coord 2 3, Coord 2 4, Coord 6 6, Coord 8 2, Coord 8 3, Coord 8 4]
                 , _hwalls    = [Coord 3 0, Coord 2 3, Coord 3 3, Coord 6 6, Coord 7 6]
                 , _bsize    = 10
                 , _exit     = Coord 3 0
                 , _lock     = True
                 , _gameState = Playing
                 , _level = 8
                 , _keyCount = 1
                 }
level_9 :: Game
level_9 = Game { _explorer = Coord 1 1
                 , _mummies  = [(Mummy {_mloc = Coord 6 6, _mct = 2, _mrct = 0}), (Mummy {_mloc = Coord 8 3, _mct = 2, _mrct = 0})]
                 , _trap = [Coord 2 2]
                 , _keys = [Coord 2 1]
                 , _vwalls    = [Coord 2 3, Coord 2 4, Coord 6 6, Coord 8 2, Coord 8 3, Coord 8 4]
                 , _hwalls    = [Coord 3 0, Coord 2 3, Coord 3 3, Coord 6 6, Coord 7 6]
                 , _bsize    = 10
                 , _exit     = Coord 3 0
                 , _lock     = True
                 , _gameState = Playing
                 , _level = 9
                 , _keyCount = 1
                 }
level_10 :: Game
level_10 = Game { _explorer = Coord 1 1
                 , _mummies  = [(Mummy {_mloc = Coord 6 6, _mct = 2, _mrct = 0}), (Mummy {_mloc = Coord 8 3, _mct = 2, _mrct = 0})]
                 , _trap = [Coord 2 2]
                 , _keys = [Coord 2 1]
                 , _vwalls    = [Coord 2 3, Coord 2 4, Coord 6 6, Coord 8 2, Coord 8 3, Coord 8 4]
                 , _hwalls    = [Coord 3 0, Coord 2 3, Coord 3 3, Coord 6 6, Coord 7 6]
                 , _bsize    = 10
                 , _exit     = Coord 3 0
                 , _lock     = True
                 , _gameState = Playing
                 , _level = 10
                 , _keyCount = 1
                 }                 



you_Lose :: Game
you_Lose= Game {  _explorer = Coord 0 0
                 , _mummies  = [(Mummy {_mloc = Coord 0 0, _mct = 2, _mrct = 0})]
                , _vwalls    = [Coord 0 5,Coord 0 6,Coord 1 2,Coord 2 2, Coord 2 3,Coord 2 5,Coord 2 6,Coord 4 3,Coord 5 3,Coord 3 5, Coord 3 6,Coord 5 5,Coord 6 6, Coord 7 3, Coord 8 5, Coord 8 3, Coord 8 6]
                 , _hwalls    = [Coord 1 3,Coord 1 4, Coord 0 7,Coord 2 7,Coord 2 5,Coord 5 5, Coord 5 6,Coord 5 7, Coord 4 3, Coord 4 4, Coord 7 4,Coord 8 5, Coord 8 6,Coord 8 7] --[Coord 3 0, Coord 2 3, Coord 3 3, Coord 6 6, Coord 7 6]
                  , _bsize    = 10
                 , _exit     = Coord 0 0
                 , _lock     = False
                 , _gameState = Lose
                 }

empty_game :: Game
empty_game = Game {  _explorer = Coord 0 0
                 , _mummies  = [(Mummy {_mloc = Coord 0 0, _mct = 2, _mrct = 0})]
                 , _vwalls    = []
                 , _hwalls    = []
                 , _bsize    = 10
                 , _exit     = Coord 0 0
                 , _lock     = False
                 , _gameState = Lose
}

you_Win :: Game
you_Win= Game {  _explorer = Coord 0 0
                 , _mummies  = [(Mummy {_mloc = Coord 0 0, _mct = 2, _mrct = 0})]
                , _vwalls    = [Coord 0 6,Coord 1 6,Coord 2 6,Coord 1 2,Coord 2 2, Coord 2 3,Coord 2 6,Coord 4 6, Coord 4 3,Coord 5 3,Coord 6 6,Coord 7 6, Coord 7 3,Coord 8 3]
                 , _hwalls    = [Coord 1 3,Coord 1 4, Coord 0 7,Coord 1 7, Coord 4 3, Coord 4 4, Coord 7 4,Coord 6 6] --[Coord 3 0, Coord 2 3, Coord 3 3, Coord 6 6, Coord 7 6]
                 , _bsize    = 10
                 , _exit     = Coord 0 0
                 , _lock     = False
                 , _gameState = Win
}

start_Screen :: Game
start_Screen = Game { _explorer = Coord 1 1
                 , _mummies  = [(Mummy {_mloc = Coord 0 4, _mct = 2, _mrct = 0})]
                 , _vwalls    = [Coord 0 3, Coord 2 1, Coord 4 1, Coord 1 3, Coord 3 5]
                 , _hwalls    = []
                 , _bsize    = 6
                 , _exit     = Coord 3 0
                 , _lock     = True
                 , _gameState = SelectScreen
                 }


l1 :: [Char]
l1 = " _ _ _ _ _ _ _ _ _ _ "
l2 :: [Char]
l2 = "  _ _ _ _ _ _ _ _ _ \n" ++ l1 
l3 :: [Char]
l3 = "   _ _ _ _ _ _ _ _\n" ++ l2
l4 :: [Char]
l4 = "    _ _ _ _ _ _ _\n" ++ l3
l5 :: [Char]
l5 = "     _ _ _ _ _ _\n" ++ l4 
l6 :: [Char]
l6 = "      _ _ _ _ _\n" ++ l5
l7 :: [Char]
l7 = "       _ _ _ _\n" ++ l6  
l8 :: [Char]
l8 = "        _ _ _\n" ++ l7 
l9 :: [Char]
l9 = "         _ _\n" ++ l8
l10 :: [Char]
l10 = "          _\n" ++ l9 



smiley_face :: [Char]
smiley_face = " - - - - - - -\n" ++
              "-               -\n" ++
              "-   ◍      ◍   -\n" ++
              "-               -\n" ++
              "-        ^      -\n" ++
              "-  _        _   -\n" ++
              "-    _ _ _ _    -\n" ++
              "-               -\n" ++
                "- - - - - - -"


t1,t2,t3,t4,t5 :: [Int]
t1 = [0,1,0,0,0,0,0,1,0,0,1,0,0,0,0,1,0,0,1,0,0,0,0,0,1,0,0,1,0,0,0,0,0,1,0,0,1,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,1,1,1,1,1,1,0,0,1,1,1,1,1,1,0]
t2 = [0,1,1,0,0,0,1,1,0,0,1,0,0,0,0,1,0,0,1,1,0,0,0,1,1,0,0,1,1,0,0,0,1,1,0,0,0,1,0,1,0,0,0,0,0,0,0,1,1,0,0,0,1,1,0,0,0,0,0,1,0,1,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,0,0,0]
t3 = [0,1,0,1,0,1,0,1,0,0,1,0,0,0,0,1,0,0,1,0,1,0,1,0,1,0,0,1,0,1,0,1,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,1,0,1,0,1,0,0,0,0,1,1,1,1,1,0,0,0,0,0,0,1,0,0,0,0,1,1,1,1,1,1,0]
t4 = [0,1,0,0,1,0,0,1,0,0,1,0,0,0,0,1,0,0,1,0,0,1,0,0,1,0,0,1,0,0,1,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,1,0,0,1,0,0,0,1,0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,0]
t5 = [0,1,0,0,1,0,0,1,0,0,0,1,1,1,1,0,0,0,1,0,0,1,0,0,1,0,0,1,0,0,1,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,1,0,0,1,0,0,1,0,0,0,0,0,0,0,1,0,0,1,1,1,1,1,0,0,1,1,1,1,1,1,0]


-- attributes map
theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (blueBg, V.blue `on` V.blue),
    (cyanBg, V.cyan `on` V.cyan),
    (redBg, V.red `on` V.red),
    (greenBg, V.green `on` V.green),
    (whiteBg, V.white `on` V.white),
    (blackBg, V.black `on` V.black),
    (greyBg, V.rgbColor 87 50 13 `on` V.rgbColor 87 50 30),
    (brown1Bg, V.rgbColor 150 60 0 `on` V.rgbColor 150 60 0),
    (brownBg, V.rgbColor 204 102 0 `on` V.rgbColor 204 102 0),
    (brown2Bg, V.rgbColor 255 255 255 `on` V.rgbColor 255 255 255) ,
    (yellowBg, V.yellow `on` V.yellow),
    (outerBg, V.rgbColor 240 200 120 `on` V.rgbColor 240 200 120),
    (brnbrnBg, V.rgbColor 90 30 0 `on` V.rgbColor 240 200 120), -- for hot keys box
    (B.borderAttr,      V.rgbColor 90 30 0 `on` V.rgbColor 240 200 120) -- for hot keys box
     ]


blueBg, redBg, cyanBg, whiteBg, blackBg, greenBg, greyBg, brown1Bg, brown2Bg, brownBg, yellowBg, brnbrnBg, outerBg :: AttrName
blueBg = attrName "blueBg"
cyanBg = attrName "cyanBg"
redBg = attrName "redBg"
whiteBg = attrName "whiteBg"
blackBg = attrName "blackBg"
greenBg = attrName "greenBg"
greyBg = attrName "greyBg"
brownBg = attrName "brownBg"
brown1Bg = attrName "brown1Bg"
brown2Bg = attrName "brown2Bg"
yellowBg = attrName "yellowBg"
brnbrnBg = attrName "brnbrnBg"
outerBg = attrName "outerBg"