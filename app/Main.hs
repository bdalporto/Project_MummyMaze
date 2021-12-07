module Main where

import System.Process

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
import Start (start)
import Game
import Levels
import Instructions (instructions)

main :: IO ()
main = do
    callCommand "printf '\\e[8;56;165t'"
    eventChan <- newBChan 10
    guacamole <- start
    l <- instructions
    forkIO $ forever $ do
        writeBChan eventChan Counter
        threadDelay 500000 -- decides how fast your game moves
    let buildVty = V.mkVty V.defaultConfig
    initialVty <- buildVty
    finalState <- customMain initialVty buildVty
                    (Just eventChan) Game.app (choose_Level l)--test
    case _gameState finalState of
     Levels.SelectScreen -> main
     _ -> return ()
    putStrLn "done"