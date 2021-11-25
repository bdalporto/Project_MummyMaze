module Game where

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
import Levels
import PickLevel (pickLevel)
import qualified Brick.Widgets.Border as C
-- Custom event
data Counter = Counter

type Name = ()

choose_Level :: Int -> Game
choose_Level l
   | l == 1  = Levels.test
   | l == 2  = you_Win
   | l == 3  = you_Lose
   | l == 4  = test
   | l == 5  = you_Win
   | otherwise = you_Lose



app :: App Game Counter Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }

handleEvent :: Game -> BrickEvent Name Counter -> EventM Name (Next Game)
handleEvent g (AppEvent Counter)                    =  continue $ counterStep  g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g
handleEvent g (VtyEvent (V.EvKey V.KEsc []))        = halt g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'r') [])) = halt start_Screen
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
counterStep g = case _gameState g of
    Playing -> moveMummies g
    _ -> g {_lock = False}

-- Moves the mummies
moveMummies :: Game -> Game
moveMummies g | movesLeft == 0   = g {_mummies = map (moveMummy ec g) mummies, _lock = True, _gameState = gameState}
              | otherwise        = g {_mummies = map (moveMummy ec g) mummies, _gameState = gameState}
    where mummies = _mummies g
          ec = _explorer g
          new_mummies = map (moveMummy ec g) mummies
          movesLeft = foldr (\m b -> _mrct m + b) 0 new_mummies
          gameState = checkGameState g

-- Moves the mummy based on where the explorer is
moveMummy :: Coord -> Game -> Mummy -> Mummy
moveMummy c@(Coord c1 c2) g m | mrct <= 0 = m
                              | otherwise = m {_mloc = new_coord, _mrct = mrct-1}
    where
    mrct = _mrct m
    mloc@(Coord l1 l2) = _mloc m
    d1 = c1 - l1
    d2 = c2 - l2
    new_coord
        | d1 > 0 && validMove East mloc g /= mloc = validMove East mloc g
        | d1 < 0 && validMove West mloc g /= mloc = validMove West mloc g
        | d2 < 0 && validMove North mloc g /= mloc = validMove North mloc g
        | d2 > 0 && validMove South mloc g /= mloc = validMove South mloc g
        | otherwise  = mloc

-- Checks if mummy caught explorer, and returns if game is over
checkGameState :: Game -> GameState
checkGameState g = if elem (_explorer g) (map (\m -> (_mloc m)) (_mummies g)) then Lose else Playing


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
checkWin g = if (_explorer g) == (_exit g) then Win else Playing

-- Sets the mummy move counters
setMummyCounters :: Game -> [Mummy]
setMummyCounters g = map setMummyCounter mummies
    where mummies = _mummies g

-- Sets the mummy move counters
setMummyCounter :: Mummy -> Mummy
setMummyCounter m = m {_mrct = mct}
    where mct = _mct m

-- Checks if a move is valid given: direction, current coordinates, game
validMove :: MyDirection -> Coord -> Game -> Coord
validMove d c@(Coord x y) g | d == North && notElem (Coord x y) (_hwalls g) && y-1 >= 0 = Coord x (y-1)
                          | d == South && notElem (Coord x (y+1)) (_hwalls g) && y+1 < b = Coord x (y+1)
                          | d == East && notElem (Coord (x+1) y) (_vwalls g) && x+1 < b = Coord (x+1) y
                          | d == West && notElem (Coord x y) (_vwalls g) && x-1 >= 0 = Coord (x-1) y
                          | otherwise = c
        where b = _bsize g



{---------- UI CODE ----------}

-- draw the game UI
drawUI :: Game -> [Widget Name]
drawUI g = case _gameState g of
    SelectScreen -> [drawGrid g]
    Playing -> drawCharacters g ++ [drawGrid g]  ++ [drawHelp]
    Lose -> [drawGameOver True]
    Win -> [drawGrid you_Win]
{-
drawUI :: Game -> [Widget Name]
drawUI g | (_gameState g) == Playing   = (drawCharacters g) ++ [(drawGrid g)]
         | otherwise          = [(drawGrid g)]
-}

drawGameOver :: Bool -> Widget Name
drawGameOver dead =
  if dead
     then padLeftRight 4 $ withAttr gameOverAttr $ str "GAME OVER"
     else emptyWidget



drawHelp :: Widget Name
drawHelp =
  withBorderStyle BS.unicodeBold
    $ B.borderWithLabel (str "CONTROLS")
    $ padTopBottom 100
    $ hLimit 10
    $ vBox
    $ map (uncurry drawKeyInfo)
      [ ("Left"   , "←")
      , ("Right"  , "→")
      , ("up"   , "↑")
      , ("Down"   , "↓")
      , ("Restart to level select", "r")
      , ("Quit"   , "q")
      ]
drawKeyInfo :: String -> String -> Widget Name
drawKeyInfo action keys =
  padRight Max (padLeft (Pad 1) $ str action)
    <+> padLeft Max (padRight (Pad 1) $ str keys)




-- draws all of the characters
drawCharacters :: Game -> [Widget Name]
drawCharacters g = d_mums ++ [d_exp]
    where
        d_exp = drawCharacter Explorer (_explorer g)
        d_mums = map (drawCharacter CMummy . _mloc) (_mummies g)

-- draws a character given the type and location
drawCharacter :: Cell -> Coord -> Widget Name
drawCharacter ctype (Coord x y) = tCell
    where
        cellSize = 4
        rep xs = foldr (\a b -> replicate (cellSize-2) a ++ b) [] xs
        cell = vBox $ rep $ [hBox $ rep [drawCell ctype]]
        tCell = translateBy (Location (2*x*(cellSize+1)+4 ,y*(cellSize+1)+2)) cell

-- Draws the board and walls
drawGrid :: Game -> Widget Name
drawGrid g = vBox rows
  where
    cellSize = 4
    rep f n xs = map (\ a -> f (replicate n a)) xs
    b = _bsize g
    rows         = interleaveWalls (rep vBox cellSize [hBox $ cellsInRow r | r <- [0..b-1]]) hWalls
    hWalls = [hBox $ interleaveWalls (rep hBox cellSize [drawWall (_hwalls g) (Coord x y) | x <- [0..b]]) bwalls| y <- [0..b]]
    cellsInRow y = interleaveWalls (rep hBox cellSize [drawCoord (Coord x y) | x <- [0..b-1]]) (vwalls y)
    vwalls y = [drawWall (_vwalls g) (Coord x y) | x <- [0..b]]
    bwalls = [drawCell NoWall | x <- [0..b]]
    drawCoord    = drawCell . cellAt
    cellAt (Coord x y)
      | even (x + y)  = Empty0
      | otherwise           = Empty1
    drawWall walls c@(Coord c1 c2)
      | c `elem` walls      = drawCell Wall
      | otherwise          = drawCell NoWall

-- Helper function for drawGrid
interleaveWalls :: [a] -> [a] -> [a]
interleaveWalls xs (y:ys) = y : concat (zipWith (\x y -> [x]++[y]) xs ys)


data Cell = Empty0 | Empty1 | Explorer | Wall | NoWall | CMummy

drawCell :: Cell -> Widget Name
drawCell Explorer = withAttr greyBg cw
drawCell Wall = withAttr blackBg cw
drawCell NoWall = withAttr brown1Bg cw
drawCell Empty0 = withAttr brownBg cw
drawCell Empty1 = withAttr brown1Bg cw
drawCell CMummy = withAttr whiteBg cw

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
    (blackBg, V.black `on` V.black),
    (greyBg, V.rgbColor 87 50 13 `on` V.rgbColor 87 50 30),
    (brown1Bg, V.rgbColor 150 60 0 `on` V.rgbColor 150 60 0),
    (brownBg, V.rgbColor 204 102 0 `on` V.rgbColor 204 102 0),
    (brown2Bg, V.rgbColor 255 255 255 `on` V.rgbColor 255 255 255)
     ]




blueBg, redBg, cyanBg, whiteBg, blackBg, greenBg, greyBg, brown1Bg, brown2Bg, brownBg, gameOverAttr :: AttrName
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
gameOverAttr = attrName "gameOver"