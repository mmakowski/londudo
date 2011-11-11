module Londudo (
  Player (..),
  Bid (..),
  PlayerState (..),
  GameState (..),
  VisiblePlayerState (..),
  VisibleGameState (..),
  Move (..),
  chooseMove,
  makeMove  
) where

import Control.Arrow (second)

newtype Player = Player { playerId :: Int } 
               deriving (Eq, Show)

type PlayerMap a = [(Player, a)]                        

type DiceCounts = PlayerMap Int

data Bid = Bid { bidCount :: Int, bidValue :: Int } 
         deriving (Eq, Show)
                  
data VisiblePlayerState = VisiblePlayerState { revealedValues :: [Int]
                                             , countHidden :: Int
                                             , moveHistory :: [Move] }
                        deriving (Eq, Show)
type VisiblePlayerStates = PlayerMap VisiblePlayerState
                                 
data VisibleGameState = VisibleGameState { currentPlayerV :: Player 
                                         , currentBidV :: Bid 
                                         , visiblePlayerStates :: VisiblePlayerStates }
                      deriving (Eq, Show)
                               
data PlayerState = PlayerState { visibleState :: VisiblePlayerState
                               , hiddenValues :: [Int] }
                 deriving (Eq, Show)
type PlayerStates = PlayerMap PlayerState                          

data GameState = GameState { currentPlayer :: Player 
                           , currentBid :: Bid
                           , playerStates :: PlayerStates }
               | RoundFinished { loser :: Player
                               , diceCountsS :: DiceCounts }
               | GameFinished { winner :: Player }
               | RoundStart { currentPlayer :: Player
                            , playerStates :: PlayerMap PlayerState }
               deriving (Eq, Show)
                        
data Move 
     = Raise { newBid :: Bid }
     | RaiseAndRoll { newBid :: Bid, revealedValuesM :: [Int] }
     | Call 
     deriving (Eq, Show)


visibleGameState :: GameState -> VisibleGameState
visibleGameState (GameState cp bid []) = VisibleGameState cp bid []
visibleGameState (GameState cp bid ((pl, pst):psts)) = 
  VisibleGameState cp bid ((pl, vpst):vpsts)
  where
    vpst = visibleState pst
    vpsts = visiblePlayerStates $ visibleGameState (GameState cp bid psts)

chooseMove :: Player -> PlayerState -> VisibleGameState -> Move
chooseMove p ps gs = if countOfVisible (bidValue currentBid) gs >= bidCount currentBid 
                      then raiseCount currentBid
                      else Call
  where
    currentBid = currentBidV gs

makeMove :: Move -> GameState -> GameState
makeMove m@(Raise newBid) gs@(GameState cp bid psts) = 
  GameState (nextPlayer gs) newBid (addMove m cp psts)
makeMove Call gs@(GameState cp bid psts) = RoundFinished loser counts
  where
    loser = if bidIsValid gs then cp else previousPlayer gs
    counts = (takeDiceFrom loser . diceCounts) gs

bidIsValid :: GameState -> Bool
bidIsValid (GameState cp bid psts) = (countOf $ bidValue bid) psts >= bidCount bid
  where
    countOf v [] = 0
    countOf v ((_, pst):psts) = countOf' v pst + countOf v psts
    countOf' v pst = 
      (length . filter (isValue v)) $ hiddenValues pst ++ revealedValues (visibleState pst)

diceCounts :: GameState -> DiceCounts
diceCounts gs@(RoundFinished _ dc) = dc
diceCounts gs = (map (second stateToDiceCount) . playerStates) gs
  where
    stateToDiceCount :: PlayerState -> Int  
    stateToDiceCount s = let vs = visibleState s 
                         in length (revealedValues vs) + countHidden vs 

takeDiceFrom :: Player -> DiceCounts -> DiceCounts
takeDiceFrom p = map $ takeIf p
  where
    takeIf p m@(p', c) = if p' == p then (p', c - 1) else m
    
-- navigating player list

nextPlayer :: GameState -> Player
nextPlayer = selectPlayer id

previousPlayer :: GameState -> Player
previousPlayer = selectPlayer reverse

selectPlayer :: ([Player] -> [Player]) -> GameState -> Player
selectPlayer trans (GameState cp _ psts) = next cp $ (cycle . trans . map fst) psts
  where
    next :: Player -> [Player] -> Player
    next cp (p1:rest@(p2:ps))
      | cp == p1 = p2
      | otherwise = next cp rest

-- player state modifications

addMove :: Move -> Player -> PlayerMap PlayerState -> PlayerMap PlayerState
addMove _ _ p = p

countOfVisible :: Int -> VisibleGameState -> Int
countOfVisible v = length . filter (isValue v) . allRevealedValues 
                   
allRevealedValues :: VisibleGameState -> [Int]
allRevealedValues =  concatMap (revealedValues . snd) . visiblePlayerStates
                                      
isValue :: Int -> Int -> Bool
isValue base comp = comp `elem` [base, 1]

raiseCount :: Bid -> Move
raiseCount (Bid count value) = Raise $ Bid (count + 1) value

{-
visibleGameState (GameState (Player 2) (Bid 4 6) [((Player 1), (PlayerState (VisiblePlayerState [6, 1] 3 [(RaiseAndRoll (Bid 4 6) [6, 1])]) [5, 3, 3])), ((Player 2), (PlayerState (VisiblePlayerState [] 5 []) [5,4,4,3,2]))])
-}