module Main where

import Brick
import Brick.BChan
import qualified Graphics.Vty as V

import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay, forkIO)
import Data.Maybe (fromMaybe)

ui :: Widget ()
ui = (withAttr blueBg $ str "Hello, world!!")

main :: IO ()
main = do
    eventChan <- newBChan 10
    forkIO $ forever $ do
        writeBChan eventChan Counter
        threadDelay 500000 -- decides how fast your game moves
    let buildVty = V.mkVty V.defaultConfig
    initialVty <- buildVty
    finalState <- customMain initialVty buildVty
                    (Just eventChan) app test
    putStrLn "done"

-- Custom event
data Counter = Counter

type Name = ()

data Game = Game
    { _explorer :: Coord
    , _mummies  :: [Mummy]
    , _vwalls    :: [Coord]
    , _hwalls    :: [Coord]
    , _bsize    :: Int
    , _exit     :: Coord
    , _lock    :: Bool
    , _gameState :: GameState
    }

test :: Game
test = Game { _explorer = Coord 1 1
                 , _mummies  = [(Mummy {_mloc = (Coord 6 6), _mct = 2, _mrct = 0}), (Mummy {_mloc = (Coord 8 3), _mct = 2, _mrct = 0})]
                 , _vwalls    = [(Coord 2 3), (Coord 2 4), (Coord 6 6), (Coord 8 2), (Coord 8 3), (Coord 8 4)]
                 , _hwalls    = [(Coord 3 0), (Coord 2 3), (Coord 3 3), (Coord 6 6), (Coord 7 6)]
                 , _bsize    = 10
                 , _exit     = Coord 3 0
                 , _lock     = True
                 , _gameState = Playing
                 }

level_1 :: Game
level_1 = Game { _explorer = Coord 1 1
                 , _mummies  = [(Mummy {_mloc = (Coord 0 4), _mct = 2, _mrct = 0})]
                 , _vwalls    = [(Coord 0 3), (Coord 2 1), (Coord 4 1), (Coord 1 3), (Coord 3 5)]
                 , _hwalls    = []
                 , _bsize    = 6
                 , _exit     = Coord 3 0
                 , _lock     = True
                 , _gameState = Playing
                 }

data Coord = Coord { x :: Int, y :: Int} deriving (Eq, Ord, Show)

data GameState = Win | Lose | Playing

data Mummy = Mummy
    { _mloc :: Coord
    , _mct :: Int
    , _mrct :: Int
    }

app :: App Game Counter Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }

handleEvent :: Game -> BrickEvent Name Counter -> EventM Name (Next Game)
handleEvent g (AppEvent Counter)                    = continue $ counterStep g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g
handleEvent g (VtyEvent (V.EvKey V.KEsc []))        = halt g
handleEvent g (VtyEvent (V.EvKey V.KUp []))         = continue $ moveExplorer North g
handleEvent g (VtyEvent (V.EvKey V.KDown []))       = continue $ moveExplorer South g
handleEvent g (VtyEvent (V.EvKey V.KRight []))      = continue $ moveExplorer East g
handleEvent g (VtyEvent (V.EvKey V.KLeft []))       = continue $ moveExplorer West g
handleEvent g _                                     = continue g



{---------- GAME LOGIC CODE ----------}

data MyDirection
  = North
  | South
  | East
  | West
  deriving (Eq, Show)

-- Special event function
counterStep :: Game -> Game
counterStep g = case (_gameState g) of
    Playing -> moveMummies g 
    _ -> g {_lock = False}

-- Moves the mummies
moveMummies :: Game -> Game
moveMummies g | movesLeft == 0   = g {_mummies = (map (moveMummy ec g) mummies), _lock = True, _gameState = gameState}
              | otherwise        = g {_mummies = (map (moveMummy ec g) mummies), _gameState = gameState}
    where mummies = _mummies g
          ec = _explorer g
          new_mummies = (map (moveMummy ec g) mummies)
          movesLeft = foldr (\m b -> (_mrct m) + b) 0 new_mummies
          gameState = checkGameState g

-- Moves the mummy based on where the explorer is
moveMummy :: Coord -> Game -> Mummy -> Mummy 
moveMummy c@(Coord c1 c2) g m | mrct <= 0 = m
                              | otherwise = m {_mloc = new_coord, _mrct = (mrct-1)}
    where
    mrct = _mrct m
    mloc@(Coord l1 l2) = _mloc m
    d1 = c1 - l1
    d2 = c2 - l2
    new_coord
        | d1 > 0 && not((validMove East mloc g)==mloc) = (validMove East mloc g)
        | d1 < 0 && not((validMove West mloc g)==mloc) = (validMove West mloc g)
        | d2 < 0 && not((validMove North mloc g)==mloc) = (validMove North mloc g)
        | d2 > 0 && not((validMove South mloc g)==mloc) = (validMove South mloc g)
        | otherwise  = mloc

-- Checks if mummy caught explorer, and returns if game is over
checkGameState :: Game -> GameState
checkGameState g = case (elem (_explorer g) (map (\m -> (_mloc m)) (_mummies g))) of
    True -> Lose
    False -> Playing


-- Moves the explorer based on direction
moveExplorer :: MyDirection -> Game -> Game
moveExplorer d g | _lock g   = g {_explorer = new_coords, _mummies = setMummyCounters g, _lock = False, _gameState = gameState}
                 | otherwise = g
    where 
        cur_coords@(Coord x y) = _explorer g
        new_coords = validMove d cur_coords g
        new_game = g {_explorer = new_coords}
        gameState = checkWin new_game


-- Checks if explorer won the game
checkWin :: Game -> GameState
checkWin g = case ((_explorer g) == (_exit g)) of
    True -> Win
    False -> Playing

-- Sets the mummy move counters
setMummyCounters :: Game -> [Mummy]
setMummyCounters g = (map setMummyCounter mummies)
    where mummies = _mummies g

-- Sets the mummy move counters
setMummyCounter :: Mummy -> Mummy
setMummyCounter m = m {_mrct = mct}
    where mct = _mct m

-- Checks if a move is valid given: direction, current coordinates, game
validMove :: MyDirection -> Coord -> Game -> Coord
validMove d c@(Coord x y) g | d == North && not (elem (Coord x y) (_hwalls g)) && y-1 >= 0 = Coord x (y-1)
                          | d == South && not (elem (Coord x (y+1)) (_hwalls g)) && y+1 < b = Coord x (y+1)
                          | d == East && not (elem (Coord (x+1) y) (_vwalls g)) && x+1 < b = Coord (x+1) y
                          | d == West && not (elem (Coord x y) (_vwalls g)) && x-1 >= 0 = Coord (x-1) y
                          | otherwise = c
        where b = _bsize g



{---------- UI CODE ----------}

-- draw the game UI
drawUI :: Game -> [Widget Name]
drawUI g = case (_gameState g) of
    Playing -> (drawCharacters g) ++ [(drawGrid g)]
    Lose -> [(drawGrid g)]
    Win -> (drawCharacters g)
{-
drawUI :: Game -> [Widget Name]
drawUI g | (_gameState g) == Playing   = (drawCharacters g) ++ [(drawGrid g)]
         | otherwise          = [(drawGrid g)]
-}

-- draws all of the characters
drawCharacters :: Game -> [Widget Name]
drawCharacters g = d_mums ++ [d_exp]
    where
        d_exp = drawCharacter Explorer (_explorer g)
        d_mums = map (\m -> drawCharacter CMummy (_mloc m)) (_mummies g)

-- draws a character given the type and location
drawCharacter :: Cell -> Coord -> Widget Name
drawCharacter ctype (Coord x y) = tCell
    where 
        cellSize = 4
        rep xs = foldr (\a b -> (replicate (cellSize-2) a) ++ b) [] xs
        cell = vBox $ rep $ [hBox $ rep [drawCell ctype]]
        tCell = translateBy (Location (2*x*(cellSize+1)+4 ,y*(cellSize+1)+2)) cell

-- Draws the board and walls
drawGrid :: Game -> Widget Name
drawGrid g = vBox rows
  where
    cellSize = 4
    rep f n xs = foldr (\a b -> [(f (replicate n a))] ++ b) [] xs
    b = _bsize g
    rows         = interleaveWalls (rep vBox cellSize [hBox $ cellsInRow r | r <- [0..b-1]]) hWalls
    hWalls = [hBox $ interleaveWalls (rep hBox (cellSize) [(drawWall (_hwalls g) (Coord x y)) | x <- [0..b]]) bwalls| y <- [0..b]]
    cellsInRow y = interleaveWalls (rep hBox cellSize [drawCoord (Coord x y) | x <- [0..b-1]]) (vwalls y)
    vwalls y = [drawWall (_vwalls g) (Coord x y) | x <- [0..b]]
    bwalls = [drawCell NoWall | x <- [0..b]] 
    drawCoord    = drawCell . cellAt
    cellAt (Coord x y)
      | mod (x + y) 2 == 0  = Empty0  
      | otherwise           = Empty1
    drawWall walls c@(Coord c1 c2)
      | elem c walls      = drawCell Wall
      | otherwise          = drawCell NoWall

-- Helper function for drawGrid
interleaveWalls :: [a] -> [a] -> [a]
interleaveWalls xs (y:ys) = [y] ++ (concat (zipWith (\x y -> [x]++[y]) xs ys))


data Cell = Empty0 | Empty1 | Explorer | Wall | NoWall | CMummy

drawCell :: Cell -> Widget Name
drawCell Explorer = withAttr redBg cw
drawCell Wall = withAttr blackBg cw
drawCell NoWall = withAttr whiteBg cw
drawCell Empty0 = withAttr cyanBg cw
drawCell Empty1 = withAttr whiteBg cw
drawCell CMummy = withAttr blueBg cw

cw :: Widget Name
cw = str ".."


-- attributes map
theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (blueBg, V.blue `on` V.blue),
    (cyanBg, V.cyan `on` V.cyan),
    (redBg, V.red `on` V.red),
    (greenBg, V.green `on` V.green),
    (whiteBg, V.white `on` V.white),
    (blackBg, V.black `on` V.black) ]

blueBg, redBg, cyanBg, whiteBg, blackBg, greenBg:: AttrName
blueBg = attrName "blueBg"
cyanBg = attrName "cyanBg"
redBg = attrName "redBg"
whiteBg = attrName "whiteBg"
blackBg = attrName "blackBg"
greenBg = attrName "greenBg"


