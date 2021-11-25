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

data GameState = SelectScreen | Win | Lose | Playing 

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
                 , _mummies  = [(Mummy {_mloc = Coord 0 4, _mct = 2, _mrct = 0})]
                 , _vwalls    = [Coord 0 3, Coord 2 1, Coord 4 1, Coord 1 3, Coord 3 5]
                 , _hwalls    = []
                 , _bsize    = 6
                 , _exit     = Coord 3 0
                 , _lock     = True
                 , _gameState = Playing
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