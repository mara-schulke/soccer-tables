module Tables where

import Data.List.Split

-- The mathematical factorial function
factorial :: Int -> Int
factorial n = product [1..n]

-- The binomial coefficient function
binomialCoefficient :: Int -> Int -> Int
binomialCoefficient n k | k > n = 0
                        | k < 0 = 0
                        | n < 0 = 0
                        | n > k = (factorial n) `div` ((factorial k) * (factorial (n - k))) 

-- A point
type Point = Float

-- A team name
type Team = Char

-- A single match
data Game = Game { teamNames :: (Team, Team), gameResults :: (Point, Point) }

instance Show Game where
  show (Game teams points) = [fst teams] ++ ":" ++ [snd teams] ++ " -> " ++ show (fst points) ++ ":" ++ show (snd points)

-- All matches played at one day
type Gameday = [Game]

-- Final results paired with a team name
data TeamResult = TeamResult { teamName :: Team, points :: Point }

instance Show TeamResult where
  show (TeamResult team points) = [team] ++ " -> " ++ show points

-- All results combined
data Table = Table [TeamResult] [Gameday]

instance Show Table where
  show (Table res days) = "TABELLE: \n    ERGEBNISSE\n" ++ ((unlines . map (\r -> "        " ++ show r)) $ res) ++ "\n" ++ ((unlines . map (\day -> "    SPIELTAG\n" ++ (((unlines . map (\game -> "        " ++ show game))) $ day) )) $ days)

-- Points for a win
win :: Point
win = 1

-- Points for a draw
draw :: Point
draw = 0.5

-- Points for a lose
lose :: Point
lose = 0

-- The teams
teams :: [Team]
teams = ['A', 'B', 'C', 'D']

-- The amount of games is the number of combinations of exactly 2 teams
games :: Int
games = binomialCoefficient (length teams) 2

-- Amount of games played per gameday
gamesPerDay :: Int
gamesPerDay = 2

-- Amount of gamedays needed to play all games
amountOfGamedays :: Int
amountOfGamedays = ceiling ((fromIntegral games) / (fromIntegral gamesPerDay))

-- All combinations of teams
combinationsOfTeams :: [(Team, Team)]
combinationsOfTeams = map (\x -> (x !! 0, x !! 1)) [x| x <- mapM (const teams) [1..2], head x < head (tail x) ]

-- All possible Results for a game
possibleResults :: [(Point, Point)]
possibleResults = [(win, lose), (lose, win), (draw, draw)]

-- fst (allCombos !! 0) == combinationOfTeams !! 0
allCombos :: [[Game]]
allCombos = map (\team -> map (\res -> Game team res) possibleResults) combinationsOfTeams

carthesianProduct :: [Game] -> [Game] -> [[Game]]
carthesianProduct xs ys = [[x] ++ [y] | x <- xs, y <- ys ]

-- Combine all values from the first list and the second list to generate pairs
-- ['X', 'Y'] ['A', 'B'] would return [['A','B','X'], ['A','B','Y']]
combine :: [Game] -> [Game] -> [[Game]]
combine v l = [l ++ [x] | x <- v]

-- THIS SHOULD BE RECURSIVE BUT I AM TO DUMB TO DO THAT --
start = carthesianProduct (allCombos !! 0) (allCombos !! 1)
level2 = concat (map (combine (allCombos !! 2)) start)
level3 = concat (map (combine (allCombos !! 3)) level2)
level4 = concat (map (combine (allCombos !! 4)) level3)
level5 = concat (map (combine (allCombos !! 5)) level4)
----------------------------------------------------------

possibleStreaks :: [[Game]]
possibleStreaks = level5

possibleGamedayStreaks :: [[Gameday]]
possibleGamedayStreaks = map (chunksOf gamesPerDay) possibleStreaks

teamParticipatedInGame :: Team -> Game -> Bool
teamParticipatedInGame t g = ((t == fst tn) || (t == snd tn)) where tn = (teamNames g)

pointsForTeam :: Team -> Game -> Point
pointsForTeam t g =
                let tn = (teamNames g)
                    res = (gameResults g)
                in if t == fst tn then fst res
                else if t == snd tn then snd res
                else 0

filterGamesWithTeam :: Team -> [Game] -> [Game]
filterGamesWithTeam t games = filter (teamParticipatedInGame t) games

sumPointsForTeam :: Team -> [Game] -> Point
sumPointsForTeam t g = (sum (map (pointsForTeam t) (filterGamesWithTeam t g)))

gamedaysToTeamResult :: [Gameday] -> [TeamResult]
gamedaysToTeamResult gamedays = map (\team -> TeamResult team (sumPointsForTeam team (concat gamedays))) teams

tables :: [Table]
tables = map (\x -> (Table (gamedaysToTeamResult x) x)) possibleGamedayStreaks

printTables :: IO ()
printTables = do
        putStrLn $ (unlines . map show) $ tables

main :: IO ()
main = do
    printTables
----------------------------------------------------------------------------
-- type TableEntry = (Point, [Point])
-- type Table = [TableEntry]

-- The amount of points per table is relative to the amount of games and the points for a game
-- pointsPerTable :: Point
-- pointsPerTable = (fromIntegral games) * win

-- -- Append all values from the first list to the second list 
-- append :: [Point] -> [Point] -> [[Point]]
-- append v l = [l ++ [x] | x <- v]

-- -- Create pairs of 2 lists
-- getPairs :: [Point] -> [Point] -> [[Point]]
-- getPairs v l = concat (map (append v) [l])

-- -- Create all combinations with given values to a given level
-- getCombinations :: Int -> [Point] -> [[Point]] -> [[Point]]
-- getCombinations level values start = 
--     if (level - 1) == 0 then y
--     else getCombinations (level - 1) values y
--     where y = concat (map (getPairs values) start)

-- -- All possible values for a team in a table
-- possibleValues :: [Point]
-- possibleValues = map (*0.5) (take (games + 1) [0..])
-- This is the right implementation tracking the different play days.
-- toTableEntry :: [Point] -> TableEntry
-- toTableEntry p = (sum p, p)
-- This is just a temp solution ignoring the single games
-- toTableEntry :: Point -> TableEntry
-- toTableEntry p = (p, [])


-- allUnsortedTables :: [[Point]]
-- allUnsortedTables = getCombinations (teams - 1) possibleValues [[x] | x <- possibleValues]

-- tables :: [[Point]]
-- tables = 
--     map sort (filter (\x -> sum x == pointsPerTable) allUnsortedTables)
