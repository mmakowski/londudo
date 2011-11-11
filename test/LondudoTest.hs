module LondudoTest where
import Londudo
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit hiding (Test)

allTests :: [Test]
allTests = [ obviousMoves, obviouslyWrongMoves, stateTransitions ]

obviousMoves = testGroup "moves which are clearly optimal" [
  testCase "calls if the total number of hidden and visible dice is lower than the bid" (
     Call @=? chooseMove p1 (PlayerState oneHiddenDice [1]) 
                         (VisibleGameState p1 (Bid 3 2) 
                                           [ (p1, oneHiddenDice) 
                                           , (p2, oneRevealedDice (Bid 3 2))
                                           ]))
  ]

obviouslyWrongMoves = testGroup "negative tests for moves which are clearly wrong" [
  testCase "does not call if revealed is higher than bid and can raise" (
     Call @/=? chooseMove p1 (PlayerState oneHiddenDice [1])
                          (VisibleGameState p1 (Bid 1 2)
                                            [ (p1, oneHiddenDice)
                                            , (p2, oneRevealedDice (Bid 1 2))
                                            ]))
  ]

stateTransitions = testGroup "applying moves to game states" [
  testCase "calling player loses a dice if the bid is correct" (
    [(p1, 1), (p2, 2)] @=? (diceCountsS . makeMove Call . gameStateWithCurrentPlayer) p1),
  testCase "called player loses a dice if the bid is incorrect" (
    [(p1, 2), (p2, 1), (p3, 2)] @=? (diceCountsS . makeMove Call) gameStateWithThreePlayers),
  testCase "current player wraps after raise" (
    p1 @=? (currentPlayer . makeMove (Raise (Bid 2 2)) . gameStateWithCurrentPlayer) p2)
  testCase "move is added to players move list" (
    (Raise (Bid 2 2)) @=? playerState
  ]

-- convenience definitions to make tests read better

nHiddenDie :: Int -> VisiblePlayerState
nHiddenDie n = VisiblePlayerState [] n []

oneHiddenDice :: VisiblePlayerState
oneHiddenDice = nHiddenDie 1

twoHiddenDie :: VisiblePlayerState
twoHiddenDie = nHiddenDie 2

nDie :: Int -> [Int] -> PlayerState
nDie n vs = PlayerState (nHiddenDie n) vs

oneRevealedDice :: Bid -> VisiblePlayerState
oneRevealedDice b = VisiblePlayerState [1] 0 [Raise b]

p1 = Player 1
p2 = Player 2
p3 = Player 3

gameStateWithCurrentPlayer :: Player -> GameState
gameStateWithCurrentPlayer p = GameState p (Bid 1 2) 
                                         [ (p1, nDie 2 [1, 1])
                                         , (p2, nDie 2 [1, 1])
                                         ]

gameStateWithThreePlayers :: GameState
gameStateWithThreePlayers = GameState p3 (Bid 7 1) 
                                      [ (p1, nDie 2 [1, 1])
                                      , (p2, nDie 2 [1, 1])
                                      , (p3, nDie 2 [1, 1])                      
                                      ]
(@/=?) :: (Eq a, Show a) => a -> a -> Assertion
a @/=? b = assertBool ("expected something other than " ++ (show a)) (a /= b)
