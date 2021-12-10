import Common
import Test.Tasty
import Game as G
import Levels as L

main :: IO ()
main = runTests [testMoves]

testMoves ::  Score -> TestTree
testMoves sc = testGroup "testMoves"
 [ scoreTest (G.checkWin, empty_game, L.Playing, 1, "test-gamestate-emptygame")
 ]
 where	
	scoreTest :: (Show b, Eq b) => (a -> b, a, b, Int, String) -> TestTree
	scoreTest (f, x, r, n, msg) = scoreTest' sc (return . f, x, r, n, msg)
