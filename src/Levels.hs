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
    , _trap :: Coord
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

data GameState = SelectScreen | Win | Lose | Playing | Over

data Mummy = Mummy
    { _mloc :: Coord
    , _mct :: Int
    , _mrct :: Int
    }


test :: Game
test = Game { _explorer = Coord 1 1
                 , _mummies  = [(Mummy {_mloc = Coord 6 6, _mct = 2, _mrct = 0}), (Mummy {_mloc = Coord 8 3, _mct = 2, _mrct = 0})]
                 , _trap = Coord 2 2
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
level_1 = Game { _explorer = Coord 1 1
                 , _mummies  = [(Mummy {_mloc = Coord 6 6, _mct = 2, _mrct = 0}), (Mummy {_mloc = Coord 8 3, _mct = 2, _mrct = 0})]
                 , _trap = Coord 2 2
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
level_2 :: Game
level_2 = Game { _explorer = Coord 1 1
                 , _mummies  = [(Mummy {_mloc = Coord 6 6, _mct = 2, _mrct = 0}), (Mummy {_mloc = Coord 8 3, _mct = 2, _mrct = 0})]
                 , _trap = Coord 2 2
                 , _keys = [Coord 2 1]
                 , _vwalls    = [Coord 2 3, Coord 2 4, Coord 6 6, Coord 8 2, Coord 8 3, Coord 8 4]
                 , _hwalls    = [Coord 3 0, Coord 2 3, Coord 3 3, Coord 6 6, Coord 7 6]
                 , _bsize    = 10
                 , _exit     = Coord 3 0
                 , _lock     = True
                 , _gameState = Playing
                 , _level = 2
                 , _keyCount = 1
                 }
level_3 :: Game
level_3 = Game { _explorer = Coord 1 1
                 , _mummies  = [(Mummy {_mloc = Coord 6 6, _mct = 2, _mrct = 0}), (Mummy {_mloc = Coord 8 3, _mct = 2, _mrct = 0})]
                 , _trap = Coord 2 2
                 , _keys = [Coord 2 1]
                 , _vwalls    = [Coord 2 3, Coord 2 4, Coord 6 6, Coord 8 2, Coord 8 3, Coord 8 4]
                 , _hwalls    = [Coord 3 0, Coord 2 3, Coord 3 3, Coord 6 6, Coord 7 6]
                 , _bsize    = 10
                 , _exit     = Coord 3 0
                 , _lock     = True
                 , _gameState = Playing
                 , _level = 3
                 , _keyCount = 1
                 }
level_4 :: Game
level_4 = Game { _explorer = Coord 1 1
                 , _mummies  = [(Mummy {_mloc = Coord 6 6, _mct = 2, _mrct = 0}), (Mummy {_mloc = Coord 8 3, _mct = 2, _mrct = 0})]
                 , _trap = Coord 2 2
                 , _keys = [Coord 2 1]
                 , _vwalls    = [Coord 2 3, Coord 2 4, Coord 6 6, Coord 8 2, Coord 8 3, Coord 8 4]
                 , _hwalls    = [Coord 3 0, Coord 2 3, Coord 3 3, Coord 6 6, Coord 7 6]
                 , _bsize    = 10
                 , _exit     = Coord 3 0
                 , _lock     = True
                 , _gameState = Playing
                 , _level = 4
                 , _keyCount = 1
                 }   
level_5 :: Game
level_5 = Game { _explorer = Coord 1 1
                 , _mummies  = [(Mummy {_mloc = Coord 6 6, _mct = 2, _mrct = 0}), (Mummy {_mloc = Coord 8 3, _mct = 2, _mrct = 0})]
                 , _trap = Coord 2 2
                 , _keys = [Coord 2 1]
                 , _vwalls    = [Coord 2 3, Coord 2 4, Coord 6 6, Coord 8 2, Coord 8 3, Coord 8 4]
                 , _hwalls    = [Coord 3 0, Coord 2 3, Coord 3 3, Coord 6 6, Coord 7 6]
                 , _bsize    = 10
                 , _exit     = Coord 3 0
                 , _lock     = True
                 , _gameState = Playing
                 , _level = 5
                 , _keyCount = 1
                 } 
level_6 :: Game
level_6 = Game { _explorer = Coord 1 1
                 , _mummies  = [(Mummy {_mloc = Coord 6 6, _mct = 2, _mrct = 0}), (Mummy {_mloc = Coord 8 3, _mct = 2, _mrct = 0})]
                 , _trap = Coord 2 2
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
                 , _trap = Coord 2 2
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
                 , _trap = Coord 2 2
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
                 , _trap = Coord 2 2
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
                 , _trap = Coord 2 2
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