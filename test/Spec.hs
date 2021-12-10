import Common
import Test.Tasty
import Game as G
import Levels as L
import qualified Game as L

main :: IO ()
main = runTests [testLevelStartGameState, testWin, testLose,testMoves]

-- test that the starting gamestate of each level is playing
testLevelStartGameState ::  Score -> TestTree
testLevelStartGameState sc = testGroup "testLevelStartGameState"
 [ scoreTest (G.checkGameState, L.level_1, L.Playing, 1, "test-startgamestate-playing-level1")
 , scoreTest (G.checkGameState, L.level_2, L.Playing, 1, "test-startgamestate-playing-level2")
 , scoreTest (G.checkGameState, L.level_3, L.Playing, 1, "test-startgamestate-playing-level3")
 , scoreTest (G.checkGameState, L.level_4, L.Playing, 1, "test-startgamestate-playing-level4")
 , scoreTest (G.checkGameState, L.level_5, L.Playing, 1, "test-startgamestate-playing-level5")
 , scoreTest (G.checkGameState, L.level_6, L.Playing, 1, "test-startgamestate-playing-level6")
 , scoreTest (G.checkGameState, L.level_7, L.Playing, 1, "test-startgamestate-playing-level7")
 , scoreTest (G.checkGameState, L.level_8, L.Playing, 1, "test-startgamestate-playing-level8")
 , scoreTest (G.checkGameState, L.level_9, L.Playing, 1, "test-startgamestate-playing-level9")
 , scoreTest (G.checkGameState, L.level_10, L.Playing, 1, "test-startgamestate-playing-level10")
 ]
 where
        scoreTest :: (Show b, Eq b) => (a -> b, a, b, Int, String) -> TestTree
        scoreTest (f, x, r, n, msg) = scoreTest' sc (return . f, x, r, n, msg)

testLose ::  Score -> TestTree
testLose sc = testGroup "testLose"
 [ scoreTest (G.checkGameState, gLose, L.Lose , 1, "testLose")
  ,scoreTest (G.checkGameState, gLoseMummy, L.Lose , 1, "testLoseMummy")
   ,scoreTest (G.checkGameState, gLoseTrap, L.Lose , 1, "testLoseTrap")
 ]
 where
        scoreTest :: (Show b, Eq b) => (a -> b, a, b, Int, String) -> TestTree
        scoreTest (f, x, r, n, msg) = scoreTest' sc (return . f, x, r, n, msg)

testWin ::  Score -> TestTree
testWin sc = testGroup "testWin"
 [ scoreTest (G.checkWin, gWin, L.Win , 1, "testWin")
 , scoreTest (G.checkWin, gWinKey, L.Playing , 1, "testNoWinKeys")
 ]
 where
        scoreTest :: (Show b, Eq b) => (a -> b, a, b, Int, String) -> TestTree
        scoreTest (f, x, r, n, msg) = scoreTest' sc (return . f, x, r, n, msg)
testMoves ::  Score -> TestTree
testMoves sc = testGroup "tesMoves"
 [ scoreTest ((\(a,b) -> _explorer (G.moveExplorer a b),(L.West,validEmpty),  L.Coord 2 3 ,1, "testLeft"))
 , scoreTest ((\(a,b) -> _explorer (G.moveExplorer a b),(L.East,validEmpty),  L.Coord 4 3 ,1, "testRight"))
 , scoreTest ((\(a,b) -> _explorer (G.moveExplorer a b),(L.North,validEmpty),  L.Coord 3 2 ,1, "testUp"))
 , scoreTest ((\(a,b) -> _explorer (G.moveExplorer a b),(L.South,validEmpty),  L.Coord 3 4 ,1, "testDown"))
 , scoreTest ((\(a,b) -> _explorer (G.moveExplorer a b),(L.North,invalidNorth),  L.Coord 0 0 ,1, "invalidNorth"))
 , scoreTest ((\(a,b) -> _explorer (G.moveExplorer a b),(L.East,invalidEast),  L.Coord 9 0 ,1, "invalidEast"))
 , scoreTest ((\(a,b) -> _explorer (G.moveExplorer a b),(L.West, invalidWest),  L.Coord 0 0 ,1, "invalidWest"))
 , scoreTest ((\(a,b) -> _explorer (G.moveExplorer a b),(L.South,invalidSouth),  L.Coord 0 9 ,1, "invalidSouth"))
 , scoreTest ((\(a,b) -> _explorer (G.moveExplorer a b),(L.South,invalidWalls),  L.Coord 0 0 ,1, "invalidSouthWall"))
 , scoreTest ((\(a,b) -> _explorer (G.moveExplorer a b),(L.North,invalidWalls),  L.Coord 0 0 ,1, "invalidNorthWall"))
 , scoreTest ((\(a,b) -> _explorer (G.moveExplorer a b),(L.East ,invalidWalls),  L.Coord 0 0 ,1, "invalidEastWall"))
 , scoreTest ((\(a,b) -> _explorer (G.moveExplorer a b),(L.West,invalidWalls),  L.Coord 0 0 ,1, "invalidWestWall"))
 ]
 where
        scoreTest :: (Show b, Eq b) => (a -> b, a, b, Int, String) -> TestTree
        scoreTest (f, x, r, n, msg) = scoreTest' sc (return . f, x, r, n, msg)

gWin = L.Game { _explorer = Coord 1 (-1)
				,_keys = []
				, _keyCount = 0
                 , _mummies  = [(Mummy {_mloc = Coord 0 0, _mct = 2, _mrct = 0})]
                , _vwalls    = [Coord 0 6,Coord 1 6,Coord 2 6,Coord 1 2,Coord 2 2, Coord 2 3,Coord 2 6,Coord 4 6, Coord 4 3,Coord 5 3,Coord 6 6,Coord 7 6, Coord 7 3,Coord 8 3]
                 , _hwalls    = [Coord 1 3,Coord 1 4, Coord 0 7,Coord 1 7, Coord 4 3, Coord 4 4, Coord 7 4,Coord 6 6] --[Coord 3 0, Coord 2 3, Coord 3 3, Coord 6 6, Coord 7 6]
                 , _bsize    = 10
                 , _exit     = Coord 1 0
				 , _trap = [Coord 0 0]
                 , _lock     = False
                 , _gameState = Win
}
gWinKey = L.Game { _explorer = Coord 1 (-1)
				,_keys = []
				, _keyCount = 1
                 , _mummies  = [(Mummy {_mloc = Coord 0 0, _mct = 2, _mrct = 0})]
                , _vwalls    = [Coord 0 6,Coord 1 6,Coord 2 6,Coord 1 2,Coord 2 2, Coord 2 3,Coord 2 6,Coord 4 6, Coord 4 3,Coord 5 3,Coord 6 6,Coord 7 6, Coord 7 3,Coord 8 3]
                 , _hwalls    = [Coord 1 3,Coord 1 4, Coord 0 7,Coord 1 7, Coord 4 3, Coord 4 4, Coord 7 4,Coord 6 6] --[Coord 3 0, Coord 2 3, Coord 3 3, Coord 6 6, Coord 7 6]
                 , _bsize    = 10
                 , _exit     = Coord 1 0
				 , _trap = [Coord 0 0]
                 , _lock     = False
                 , _gameState = Win
}

gLose = L.Game { _explorer = Coord 0 0
                 , _mummies  = [(Mummy {_mloc = Coord 0 0, _mct = 2, _mrct = 0})]
                , _vwalls    = [Coord 0 6,Coord 1 6,Coord 2 6,Coord 1 2,Coord 2 2, Coord 2 3,Coord 2 6,Coord 4 6, Coord 4 3,Coord 5 3,Coord 6 6,Coord 7 6, Coord 7 3,Coord 8 3]
                 , _hwalls    = [Coord 1 3,Coord 1 4, Coord 0 7,Coord 1 7, Coord 4 3, Coord 4 4, Coord 7 4,Coord 6 6] --[Coord 3 0, Coord 2 3, Coord 3 3, Coord 6 6, Coord 7 6]
                 , _bsize    = 10
                 , _exit     = Coord 0 0
				 , _trap = [Coord 0 0]
                 , _lock     = False
                 , _gameState = Lose
}
gLoseTrap = L.Game { _explorer = Coord 0 0
                 , _mummies  = [(Mummy {_mloc = Coord 1 0, _mct = 2, _mrct = 0})]
                , _vwalls    = [Coord 0 6,Coord 1 6,Coord 2 6,Coord 1 2,Coord 2 2, Coord 2 3,Coord 2 6,Coord 4 6, Coord 4 3,Coord 5 3,Coord 6 6,Coord 7 6, Coord 7 3,Coord 8 3]
                 , _hwalls    = [Coord 1 3,Coord 1 4, Coord 0 7,Coord 1 7, Coord 4 3, Coord 4 4, Coord 7 4,Coord 6 6] --[Coord 3 0, Coord 2 3, Coord 3 3, Coord 6 6, Coord 7 6]
                 , _bsize    = 10
				 , _trap = [Coord 0 0]
                 , _exit     = Coord 0 0
                 , _lock     = False
                 , _gameState = Playing
}
gLoseMummy = L.Game { _explorer = Coord 0 0
                 , _mummies  = [(Mummy {_mloc = Coord 0 0, _mct = 2, _mrct = 0})]
                , _vwalls    = [Coord 0 6,Coord 1 6,Coord 2 6,Coord 1 2,Coord 2 2, Coord 2 3,Coord 2 6,Coord 4 6, Coord 4 3,Coord 5 3,Coord 6 6,Coord 7 6, Coord 7 3,Coord 8 3]
                 , _hwalls    = [Coord 1 3,Coord 1 4, Coord 0 7,Coord 1 7, Coord 4 3, Coord 4 4, Coord 7 4,Coord 6 6] --[Coord 3 0, Coord 2 3, Coord 3 3, Coord 6 6, Coord 7 6]
                 , _bsize    = 10
                 , _exit     = Coord 0 0
				 
                 , _lock     = False
                 , _gameState = Playing
}

validEmpty = L.Game { _explorer = Coord 3 3
				,_keys = []
				, _keyCount = 0
                 , _mummies  = []
                , _vwalls    = []
                 , _hwalls    = [] --[Coord 3 0, Coord 2 3, Coord 3 3, Coord 6 6, Coord 7 6]
                 , _bsize    = 10
                 , _exit     = Coord 1 0
				 , _trap = [Coord 0 0]
                 , _lock     = True
				 , _level = 1
                 , _gameState = Playing
}
invalidNorth = L.Game { _explorer = Coord 0 0
				,_keys = []
				, _keyCount = 0
                 , _mummies  = []
                , _vwalls    = []
                 , _hwalls    = [] --[Coord 3 0, Coord 2 3, Coord 3 3, Coord 6 6, Coord 7 6]
                 , _bsize    = 10
                 , _exit     = Coord 1 0
				 , _trap = [Coord 0 0]
                 , _lock     = True
				 , _level = 1
                 , _gameState = Playing
}
invalidSouth = L.Game { _explorer = Coord 0 9
				,_keys = []
				, _keyCount = 0
                 , _mummies  = []
                , _vwalls    = []
                 , _hwalls    = [] --[Coord 3 0, Coord 2 3, Coord 3 3, Coord 6 6, Coord 7 6]
                 , _bsize    = 10
                 , _exit     = Coord 1 0
				 , _trap = [Coord 0 0]
                 , _lock     = True
				 , _level = 1
                 , _gameState = Playing
}
invalidEast = L.Game { _explorer = Coord 9 0
				,_keys = []
				, _keyCount = 0
                 , _mummies  = []
                , _vwalls    = []
                 , _hwalls    = [] --[Coord 3 0, Coord 2 3, Coord 3 3, Coord 6 6, Coord 7 6]
                 , _bsize    = 10
                 , _exit     = Coord 1 0
				 , _trap = [Coord 0 0]
                 , _lock     = True
				 , _level = 1
                 , _gameState = Playing
}
invalidWest = L.Game { _explorer = Coord 0 0
				,_keys = []
				, _keyCount = 0
                 , _mummies  = []
                , _vwalls    = []
                 , _hwalls    = [] --[Coord 3 0, Coord 2 3, Coord 3 3, Coord 6 6, Coord 7 6]
                 , _bsize    = 10
                 , _exit     = Coord 1 0
				 , _trap = [Coord 0 0]
                 , _lock     = True
				 , _level = 1
                 , _gameState = Playing
}
invalidWalls = L.Game { _explorer = Coord 0 0
				,_keys = []
				, _keyCount = 0
                 , _mummies  = []
                , _vwalls    = [Coord 0 0,Coord 1 0, Coord 0 1]
                 , _hwalls    = [Coord 0 0,Coord 1 0, Coord 0 1] --[Coord 3 0, Coord 2 3, Coord 3 3, Coord 6 6, Coord 7 6]
                 , _bsize    = 10
                 , _exit     = Coord 1 0
				 , _trap = [Coord 0 0]
                 , _lock     = True
				 , _level = 1
                 , _gameState = Playing
}