module Tables where

import Data.Sort
import Data.List.Split

-- The mathematical factorial function
factorial :: Int -> Int
factorial n = product [1..n]

-- The carthesian product of two lists
carthesianProduct :: [a] -> [a] -> [[a]]
carthesianProduct xs ys = [[x] ++ [y] | x <- xs, y <- ys ]

-- Combine all values from the first list and the second list to generate pairs
-- ['X', 'Y'] ['A', 'B'] would return [['A','B','X'], ['A','B','Y']]
combine :: [a] -> [a] -> [[a]]
combine v l = [l ++ [x] | x <- v]

-- The binomial coefficient function
binomialCoefficient :: Int -> Int -> Int
binomialCoefficient n k | k > n = 0
                        | k < 0 = 0
                        | n < 0 = 0
                        | n > k = (factorial n) `div` ((factorial k) * (factorial (n - k))) 

-- Returns all elements except last from list
takeAllExceptLast :: [a] -> [a]
takeAllExceptLast xs = take ((length xs) - 1) xs

-- Returns last element from list
takeLast :: [a] -> a
takeLast xs = (take 1 (reverse xs)) !! 0

-- A point
type Point = Float

-- A team name
type Team = Char

-- A single match
data Game = Game { teamNames :: (Team, Team), gameResults :: (Point, Point) }

instance Show Game where
  show (Game teams points) = show (fst points) ++ ":" ++ show (snd points)

-- All matches played at one day
type Gameday = [Game]

-- Final results paired with a team name
data TeamResult = TeamResult { teamName :: Team, points :: Point }

instance Show TeamResult where
  show (TeamResult team points) = [team] ++ ":" ++ show points

instance Eq TeamResult where
  (TeamResult _ p1) == (TeamResult _ p2) = p1 == p2

instance Ord TeamResult where
  (TeamResult _ p1) `compare` (TeamResult _ p2) = p1 `compare` p2

-- All results combined
data Table = Table { endResults :: [TeamResult], gamedays :: [Gameday] }

instance Show Table where
  show (Table res days) = takeAllExceptLast ((concat (map showRes res)) ++ (concat (map formatGameday days)))
                          where showRes r = show r ++ ","
                                formatGameday day = concat (map (\g -> show g ++ " - ") (takeAllExceptLast day)) ++ show (takeLast day) ++ ","

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
combinationsOfTeams = map (\(x:y:xs) -> (x,y)) [x| x <- mapM (const teams) [1..2], head x < head (tail x) ]

-- All possible Results for a game
possibleResults :: [(Point, Point)]
possibleResults = [(win, lose), (lose, win), (draw, draw)]

-- All combinations of matches and results
-- The first item holds all possible results for the first match eg. "A vs B" and so on
allCombinations :: [[Game]]
allCombinations = map (\team -> map (\res -> Game team res) possibleResults) combinationsOfTeams

-- Generates all possible streaks
-- It takes all possible Values, the offset to the actual next values in the possible Values list, and base to append to
generateAllPossibleStreaks :: [[Game]] -> Int -> [[Game]] -> [[Game]]
generateAllPossibleStreaks pv offset xs =
                if (length pv) - 1 == offset then ys
                else generateAllPossibleStreaks pv (offset + 1) ys
                where ys = concat (map (combine (allCombinations !! offset)) xs)

-- The genereated possible streaks
possibleStreaks :: [[Game]]
possibleStreaks = generateAllPossibleStreaks allCombinations 0 [[]]

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

filterGamesByTeam :: Team -> [Game] -> [Game]
filterGamesByTeam t games = filter (teamParticipatedInGame t) games

sumPointsForTeam :: Team -> [Game] -> Point
sumPointsForTeam t g = (sum (map (pointsForTeam t) (filterGamesByTeam t g)))

gamedaysToTeamResult :: [Gameday] -> [TeamResult]
gamedaysToTeamResult gamedays = reverse (sort (map (\team -> TeamResult team (sumPointsForTeam team (concat gamedays))) teams))

tables :: [Table]
tables = map (\x -> (Table (gamedaysToTeamResult x) x)) possibleGamedayStreaks

printCSVHeaders :: IO ()
printCSVHeaders = do
                putStr $ (concat . map toTeamNumberHeader) teamNumbers -- Prints a number for each team
                putStr $ takeAllExceptLast (concat (formatGamedayHeaders exampleGamedays))
                putStr "\n"
                where teamNumbers = map fst (zip [1..] teams)
                      toTeamNumberHeader i = show i ++ ","

                      exampleGamedays = gamedays (tables !! 0)
                      formatGamedayHeaders days = map (\(i,d) -> formatGameday i d) (zip [1..]days)
                      formatGameday i day = "Spieltag " ++ show i ++ " (" ++ concat (map (\g -> formatGame g ++ " - ") (takeAllExceptLast day)) ++ formatGame (takeLast day) ++ "),"
                      formatGame g = [fst tn] ++ ":" ++ [snd tn] where tn = (teamNames g)


printTables :: IO ()
printTables = do
        putStrLn $ (unlines . map show) $ tables

main :: IO ()
main = do
    printCSVHeaders
    printTables