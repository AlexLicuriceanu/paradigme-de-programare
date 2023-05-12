import Data.List

-- 9.1.1
data ChessResult = Win | Draw | Loss

-- 9.1.2
instance Show ChessResult where
    show Win = "Win"
    show Draw = "Draw"
    show Loss = "Loss"

-- 9.1.3
points :: ChessResult -> Float
points Win = 1.0
points Draw = 0.5
points Loss = 0.0

-- 9.1.4
score :: [ChessResult] -> Float
score results = sum (map points results)

-- 9.1.5
ranking :: [[ChessResult]] -> [Float]
ranking results = reverse (sort (map score results))

-- 9.1.6
-- player_earnings = player points * prize money / total points
earnings :: [[ChessResult]] -> Float -> [Float]
earnings playerScores prizeMoney = map playerEarnings playerPoints
    where
        totalPoints = sum (map score playerScores)
        playerPoints = map score playerScores
        playerEarnings points = points * prizeMoney / totalPoints

-- 9.2.0
data Player = Player { name :: String, elo :: Float, tournamentGames :: [ChessResult] } deriving Show
data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show
type Tournament = Tree Player

-- 9.2.1
-- Two players are considered equal if their score is the same. Alternatively, 2 players are different if their scores are different
instance Eq Player where
    p1 == p2 = score (tournamentGames p1) == score (tournamentGames p2)
 
-- In the same manner, players can be compared based on their score
instance Ord Player where
    p1 `compare` p2 = score (tournamentGames p1) `compare` score (tournamentGames p2)

-- 9.2.2
addResult :: ChessResult -> Player -> Player
addResult result player = player {elo = updatedElo, tournamentGames = updatedGames }
    where
        updatedGames = result : tournamentGames player
        updatedElo = (elo player) + (points result)
        
-- 9.2.3
-- odds that A beats B based on their elo
winningOdds :: Float -> Float -> Float
winningOdds eloA eloB = 1 / (1 + 10** ((eloB-eloA)/400))
 
playGame :: Player -> Player -> (Player, Player)
playGame player1 player2 =
    if (winningOdds (elo player1) (elo player2)) > 0.5 then
        (player1, player2)
    else
        (player2, player1) 

-- 9.2.4
multipleMatches :: Player -> [Player] -> (Player, [Player])
multipleMatches player opponents = undefined

-- 9.2.5
groupStage :: [Player] -> [Player]
groupStage = undefined
