module Start 
(
    start
)where

import System.Exit (exitSuccess)
import Levels
import Brick
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V



app :: App (Maybe String) e ()
app = App
  { appDraw         = const [ui]
  , appHandleEvent  = handleEvent
  , appStartEvent   = return
  , appAttrMap      = const $ attrMap V.defAttr []
  , appChooseCursor = neverShowCursor
  }

ui :: Widget ()
ui = withAttr brnbrnBg  $ withBorderStyle BS.unicodeBold  
    $ B.borderWithLabel (str "") $ withAttr brnbrnBg
    $ C.center
      $ str "WELCOME TO THE MUMMY MAZE!\nENTER IF YOU DARE!\nChoose Level <Press Enter to Continue>\n\n<Press q to leave>.... with your life!"



handleEvent :: Maybe String -> BrickEvent () e -> EventM () (Next (Maybe String))
handleEvent n (VtyEvent (V.EvKey V.KEsc        _)) = halt n
handleEvent n (VtyEvent (V.EvKey (V.KChar 'q') _)) = halt n
handleEvent n (VtyEvent (V.EvKey (V.KChar 'Q') _)) = halt n
handleEvent n (VtyEvent (V.EvKey V.KEnter []))    = halt $ Just (read [])
handleEvent n _ = continue n

start :: IO String
start = defaultMain app Nothing >>= maybe exitSuccess return