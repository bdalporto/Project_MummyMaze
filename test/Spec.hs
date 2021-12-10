import Common
import Test.Tasty
import Game as G
import Levels as L

main :: IO ()
main = runTests [testLevelStartGameState]

-- test that the starting gamestate of each level is playing
testLevelStartGameState ::  Score -> TestTree
testLevelStartGameState sc = testGroup "testLevelStartGameState" 
 [ scoreTest (G.checkWin, L.level_1, L.Playing, 1, "test-startgamestate-playing-level1")
 , scoreTest (G.checkWin, L.level_2, L.Playing, 1, "test-startgamestate-playing-level2")
 , scoreTest (G.checkWin, L.level_3, L.Playing, 1, "test-startgamestate-playing-level3")
 , scoreTest (G.checkWin, L.level_4, L.Playing, 1, "test-startgamestate-playing-level4")
 , scoreTest (G.checkWin, L.level_5, L.Playing, 1, "test-startgamestate-playing-level5")
 , scoreTest (G.checkWin, L.level_6, L.Playing, 1, "test-startgamestate-playing-level6")
 , scoreTest (G.checkWin, L.level_7, L.Playing, 1, "test-startgamestate-playing-level7")
 , scoreTest (G.checkWin, L.level_8, L.Playing, 1, "test-startgamestate-playing-level8")
 , scoreTest (G.checkWin, L.level_9, L.Playing, 1, "test-startgamestate-playing-level9")
 , scoreTest (G.checkWin, L.level_10, L.Playing, 1, "test-startgamestate-playing-level10")
 ]
 where
	scoreTest :: (Show b, Eq b) => (a -> b, a, b, Int, String) -> TestTree
	scoreTest (f, x, r, n, msg) = scoreTest' sc (return . f, x, r, n, msg)
