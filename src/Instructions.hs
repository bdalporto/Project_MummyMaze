module Instructions
  ( instructions
  ) where

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
    $ B.borderWithLabel (str "INSTRUCTIONS") $ withAttr brnbrnBg
    $ C.center
      $ str ("You are a lone explorer, dressed in all grey.\n" ++
             "Escape through the many doors scattered throughout the pyramid, they can be found at any edge in a room.\n" ++
             "Take caution of the mummies! There may be more than one....\n" ++
             "Scattered around the pyramid you may find golden keys, pick them up! Or else you cannot escape.\n" ++
             "Beware of traps, they were poorly designed.... they're just black holes in the ground (we're on a budget here)\n\n"++
             "PRESS ANY KEY TO CONTINUE\t\t\t\t\t\t\t"++
             "or q if you're too scared...")



handleEvent :: Maybe String  -> BrickEvent () e -> EventM () (Next (Maybe String))
handleEvent n (VtyEvent (V.EvKey V.KEsc        _)) = halt n
handleEvent n (VtyEvent (V.EvKey (V.KChar 'q') _)) = halt n
handleEvent n (VtyEvent (V.EvKey (V.KChar 'Q') _)) = halt n
handleEvent n (VtyEvent (V.EvKey (V.KChar d) []))    =  halt $ Just (read [d])
handleEvent n _ = continue n

instructions :: IO String
instructions = defaultMain app Nothing >>= maybe exitSuccess return