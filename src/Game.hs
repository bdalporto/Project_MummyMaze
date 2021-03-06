{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
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
import qualified Brick.Widgets.Table as C
import qualified Brick.AttrMap as A
-- Custom event
data Counter = Counter

type Name = ()

choose_Level :: Int -> Game
choose_Level l = case l of
                  1 -> level_1
                  2 -> level_2
                  3 -> level_3
                  4 -> level_4
                  5 -> level_5
                  6 -> level_6
                  7 -> level_7
                  8 -> level_8
                  9 -> level_9
                  0 -> level_10
                  _ -> Game {_gameState = Over}



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
handleEvent g (VtyEvent (V.EvKey (V.KChar 'l') [])) = halt start_Screen
handleEvent g (VtyEvent (V.EvKey (V.KChar 'r') [])) = continue $ initLevel g
handleEvent g (VtyEvent (V.EvKey V.KEnter []))    = continue $ getNextLevel g
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
checkGameState g = if elem (_explorer g) (map _mloc (_mummies g)) || elem (_explorer g) (_trap g) then Lose else Playing


-- Moves the explorer based on direction
moveExplorer :: MyDirection -> Game -> Game
moveExplorer d g | _lock g   = g {_explorer = new_coords,
                                   _mummies = setMummyCounters g,
                                   _keys = _keys new_keys,
                                   _lock = False,
                                   _gameState = gameState,
                                   _keyCount = _keyCount new_keys
                                   }
                 | otherwise = g
    where
        cur_coords@(Coord x y) = _explorer g
        new_coords = validMove d cur_coords g
        new_game = g {_explorer = new_coords}
        gameState = checkWin new_game
        new_keys = checkKeys g


-- Checks if explorer won the game
checkWin :: Game -> GameState
checkWin g = if (px == -1 || py == -1 || px == _bsize g || py == _bsize g) && _keyCount g == 0 then Win else Playing
  where
    p@(Coord px py) = _explorer g
    e@(Coord ex ey) = _exit g

--Checks if the explorer is on a key / if there are any keys left
checkKeys :: Game -> Game
checkKeys g = if _explorer g `elem` _keys g
                then g { _keys = removeItem (_explorer g) (_keys g),
                        _keyCount = _keyCount g - 1}
              else g


removeItem :: Eq Coord => Coord -> [Coord] -> [Coord]
removeItem _ []                 = []
removeItem x (y:ys) | x == y    = removeItem x ys
                    | otherwise = y : removeItem x ys

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
validMove d c@(Coord x y) g | c == _exit g &&  _keyCount g == 0 && d == North && elem c (_hwalls g) && y == 0 = Coord x (y-1)
                          | (Coord x (y+1)) == _exit g &&  _keyCount g == 0 && d == South && elem (Coord x (y+1)) (_hwalls g) && y /= 0 = Coord x (y+1)
                          | (Coord (x+1) y) == _exit g &&  _keyCount g == 0 && d == East && elem (Coord (x+1) y) (_vwalls g) && x /= 0 = Coord (x+1) y
                          | c == _exit g &&  _keyCount g == 0 && d == West && elem c (_vwalls g) && x == 0 = Coord (x-1) y
                          | d == North && notElem (Coord x y) (_hwalls g) && y-1 >= 0 = Coord x (y-1)
                          | d == South && notElem (Coord x (y+1)) (_hwalls g) && y+1 < b = Coord x (y+1)
                          | d == East && notElem (Coord (x+1) y) (_vwalls g) && x+1 < b = Coord (x+1) y
                          | d == West && notElem (Coord x y) (_vwalls g) && x-1 >= 0 = Coord (x-1) y
                          | otherwise = c
        where
          b = _bsize g
          e@(Coord ex ey) = _exit g

--Returns next level
getNextLevel:: Game -> Game
getNextLevel g = case _level g of
                  1 -> level_2
                  2 -> level_3
                  3 -> level_4
                  4 -> level_5
                  5 -> level_6
                  6 -> level_7
                  7 -> level_8
                  8 -> level_9
                  9 -> level_10
                  _ -> g{ _gameState = Over}
--returns same level from start
initLevel:: Game -> Game
initLevel g = case _level g of
                  1 -> level_1
                  2 -> level_2
                  3 -> level_3
                  4 -> level_4
                  5 -> level_5
                  6 -> level_6
                  7 -> level_7
                  8 -> level_8
                  9 -> level_9
                  10 -> level_10
                  _ -> g{ _gameState = Over}


{---------- UI CODE ----------}

-- draw the game UI
drawUI :: Game -> [Widget Name]
drawUI g = case _gameState g of
    SelectScreen -> [drawGrid g]
    Playing -> (map (translateBy (Location (4,8))) (drawCharacters g)) 
                ++ [translateBy (Location (4,8)) $ drawGrid g] 
                ++ [translateBy (Location (110,8))$ drawLegend g] 
                ++ [translateBy (Location (110,25))$ drawControls g]
                ++ [translateBy (Location (1,1))$ drawTitle g]
                ++ [drawOuter g] --drawCharacters g ++ [drawGrid g]  ++ [drawControls]
    Lose -> [translateBy (Location (4,8)) $ drawGrid you_Lose] 
             ++ [ translateBy (Location (110,8))$ drawContinue g]
             ++ [translateBy (Location (1,1))$ drawTitle g]
             ++ [drawOuter g]
    Win -> [translateBy (Location (4,8)) $ drawGrid you_Win] 
            ++ [ translateBy (Location (110,8))$ drawContinue g]
            ++ [translateBy (Location (1,1))$ drawTitle g]
            ++ [drawOuter g]
    Over -> [drawGameOver True]


drawTitle:: Game -> Widget Name
drawTitle _ = vBox $ map (\c -> g c) [t1,t2,t3,t4,t5]
    where g c = hBox $ map (\e -> h e) c
          h 1 = drawCell Wall
          h 0 = drawCell Outer


drawOuter:: Game -> Widget Name
drawOuter g = vBox [hBox $ [drawCell Outer | x <-[0..b*4+45] ] | y <-[0..b*4+22]]
  where b = _bsize g

drawGameOver :: Bool -> Widget Name
drawGameOver dead =
  if dead
     then withAttr brnbrnBg $ C.center  $ str ("GAME OVER\ngood job!\n\n\n\n\n" ++ smiley_face)
     else emptyWidget

drawLegend :: Game -> Widget Name
drawLegend g =  withAttr brnbrnBg $ hLimit 50 $
    withBorderStyle BS.unicodeBold
    $ B.borderWithLabel (str "LEGEND")
    $ padTopBottom 1
    $ vBox
    $ map (\x-> padTopBottom 1 (withAttr brnbrnBg (uncurry drawLegendInfo x)))
      [ ("Explorer"   , Explorer),
        ("2-move Mummy"   , CMummy),
        ("Trap", Trap),
        ("Key", Key)
      ]

drawLegendInfo :: String -> Cell -> Widget Name
drawLegendInfo action keys = 
  padRight Max (padLeft (Pad 1) $ str action)
    <+> padLeft Max (padRight (Pad 1) $ drawCell keys)

drawControls :: Game -> Widget Name
drawControls g =  withAttr brnbrnBg $ hLimit 50 $
    withBorderStyle BS.unicodeBold
    $ B.borderWithLabel (str "CONTROLS")
    $ padTopBottom 1
    $ vBox
    $ map (\x->withAttr brnbrnBg (uncurry drawKeyInfo x))
      [ ("Left"   , "???")
      , ("Right"  , "???")
      , ("Up"   , "???")
      , ("Down"   , "???")
      , ("Level Select", "l")
      , ("Restart", "r")
      , ("Quit"   , "q")
      , ("Next Level" , "Enter")
      , ("____________________________________________________________________________________", "___________________________________________________________________________________________")
      , ( "Level: " ++ show (_level g) , checkLevel g)
      ]

drawContinue :: Game -> Widget Name
drawContinue g = withAttr brnbrnBg $ hLimit 50 $
  withBorderStyle BS.unicodeBold
    $ B.borderWithLabel (str "CONTINUE?")
    $ padTopBottom 1
    $ vBox
    $ map (\x->withAttr brnbrnBg (uncurry drawKeyInfo x))
      [  ("Next Level" , "Enter")
       , ("Quit" , "q")
       , ("Level Select" , "l")
       , ("Restart", "r")
       , ("____________________________________________________________________________________", "___________________________________________________________________________________________")
       , ( "Level: " ++ show (_level g) , checkLevel g)
      ]

checkLevel :: Game -> String
checkLevel g = case _level g of
                1 -> l1
                2 -> l2
                3 -> l3
                4 -> l4
                5 -> l5
                6 -> l6
                7 -> l7
                8 -> l8
                9 -> l9
                _ -> l10


drawKeyInfo :: String -> String -> Widget Name
drawKeyInfo action keys = 
  padRight Max (padLeft (Pad 1) $ str action)
    <+> padLeft Max (padRight (Pad 1) $ str keys)

-- draws all of the characters
drawCharacters :: Game -> [Widget Name]
drawCharacters g = d_mums ++ [d_exp] ++ d_trap ++ d_keys
    where
        d_exp = drawCharacter Explorer (_explorer g)
        d_trap = map (drawCharacter Trap) ( _trap g)
        d_mums = map (drawCharacter CMummy . _mloc) (_mummies g)
        d_keys = map (drawCharacter Key) (_keys g)


-- draws a character given the type and location
drawCharacter :: Cell -> Coord -> Widget Name
drawCharacter ctype (Coord x y) = tCell
    where
        cellSize = 4
        rep xs = foldr (\a b -> replicate (cellSize-2) a ++ b) [] xs
        cell = vBox $ rep $ [hBox $ rep [drawCell ctype]]
        tCell = translateBy (Location (2*x*(cellSize+1)+4 ,y*(cellSize+1)+2)) cell
        --tCell = translateBy (Location (2*x*(cellSize+1)+4 ,y*(cellSize+1)+6)) cell

-- Draws the board and walls
drawGrid :: Game -> Widget Name
drawGrid g = vBox rows
  where
    cellSize = 4
    rep f n xs = map (f . replicate n) xs
    b = _bsize g
    rows   = interleaveWalls (rep vBox cellSize [hBox $ cellsInRow r | r <- [0..b-1]]) hWalls
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
interleaveWalls xs (y:ys) = y : concat (zipWith (\x y -> x : [y]) xs ys)


data Cell = Empty0 | Empty1 | Explorer | Wall | NoWall | CMummy | Trap | Key | Outer

drawCell :: Cell -> Widget Name
drawCell Explorer = withAttr greyBg cw
drawCell Trap = withAttr blackBg cw
drawCell Wall = withAttr blackBg cw
drawCell NoWall = withAttr brown1Bg cw
drawCell Empty0 = withAttr brownBg cw
drawCell Empty1 = withAttr brown1Bg cw
drawCell CMummy = withAttr whiteBg cw
drawCell Key = withAttr yellowBg cw
drawCell Outer = withAttr outerBg cw

cw :: Widget Name
cw = str ".."



