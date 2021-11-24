module Main where

import Brick
import Brick.BChan
import qualified Graphics.Vty as V
--import Brick.Widgets.Image

import Control.Monad --(forever, void, transformer)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay, forkIO)
import Data.Maybe (fromMaybe)
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Border as B
import PickLevel (pickLevel)
import Game
import Levels


main :: IO ()
main = do
    eventChan <- newBChan 10
    l <- pickLevel
    forkIO $ forever $ do
        writeBChan eventChan Counter
        threadDelay 500000 -- decides how fast your game moves
    let buildVty = V.mkVty V.defaultConfig
    initialVty <- buildVty
    finalState <- customMain initialVty buildVty
                    (Just eventChan) app (choose_Level l)--test
    case _gameState finalState of
     Levels.SelectScreen -> main
     _ -> return ()
    putStrLn "done"