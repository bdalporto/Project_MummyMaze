module PickLevel
  ( pickLevel
  ) where

import System.Exit (exitSuccess)
import Levels
import Brick
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V



app :: App (Maybe Int) e ()
app = App
  { appDraw         = const [ui]
  , appHandleEvent  = handleEvent
  , appStartEvent   = return
  , appAttrMap      = const theMap
  , appChooseCursor = neverShowCursor
  }

ui :: Widget ()
ui = withAttr brnbrnBg  $ withBorderStyle BS.unicodeBold  
    $ B.borderWithLabel (str "") $ withAttr brnbrnBg
    $ C.center
      $ str "WELCOME TO THE MUMMY MAZE!\nENTER IF YOU DARE!\nChoose Level (1 - 10(0))"



handleEvent :: Maybe Int -> BrickEvent () e -> EventM () (Next (Maybe Int))
handleEvent n (VtyEvent (V.EvKey V.KEsc        _)) = halt n
handleEvent n (VtyEvent (V.EvKey (V.KChar 'q') _)) = halt n
handleEvent n (VtyEvent (V.EvKey (V.KChar 'Q') _)) = halt n
handleEvent n (VtyEvent (V.EvKey (V.KChar d) [])) =
  if d `elem` ['0' .. '9']
  then halt $ Just (read [d])
  else continue n
handleEvent n _ = continue n

pickLevel :: IO Int
pickLevel = defaultMain app Nothing >>= maybe exitSuccess return