module Board where

import Data.List
import Data.Maybe

type Board = [[Int]]
type EnumeratedBoard = [([(Int, Int)], Int)]

boardWidth :: Int
boardWidth = 7

boardHeight :: Int
boardHeight = 6 

type Player = Int -- 0: none, 1: red, 2: yellow

emptyBoard :: Board
emptyBoard = [[]]

-- Enumerate board cells
enumBoard :: Board -> EnumeratedBoard
enumBoard board = zip (map (\x -> zip x [0..]) board) [0..]

-- Player set chip into (x, y). Yet another, but more readable. 
setChip :: Int -> Int -> Player -> Board -> Board
setChip tocol torow curPlayer board = map (\(row, y) -> map (\(player, x) -> chipMap x y player) row) (enumBoard board)
  where
    chipMap x y player = if (x == tocol && y == torow) then curPlayer else player

-- Index of free row in certain collumn
freeRow :: Int -> Board -> Maybe Int
freeRow col board = case findIndex (/=0) $ map (!!col) board of
                      Nothing -> Just (boardHeight - 1)
                      Just row
                        | row /= 0  -> Just (row - 1)
                        | otherwise -> Nothing

-- All possible players moves
possibleMoves :: Player -> Board -> [Board]
possibleMoves player board = filter (not . null) $ zipWith 
         (\row j -> case row of
                      Just i -> setChip j i player board
                      Nothing -> []) indices [0..boardWidth-1]
  where
    indices = zipWith freeRow [0..boardWidth-1] (replicate boardWidth board)

-- Is it possible to make any move?
notDead :: Board -> Bool
notDead board = or $ map (\x -> any (== 0) x) board 

getDiagonals :: Board -> [[Int]]
getDiagonals board = zipWith diagonal iIndices jIndices
  where
    diagonal is js = zipWith (\i j -> (board !! i) !! j) is js
    iInd = [[2,3,4,5], [1,2,3,4,5], [0,1,2,3,4,5], [0,1,2,3,4,5], [0,1,2,3,4], [0,1,2,3]]
    jInd = [[0,1,2,3], [0,1,2,3,4], [0,1,2,3,4,5], [1,2,3,4,5,6], [2,3,4,5,6], [3,4,5,6]]
    iIndices = iInd ++ (map reverse $ reverse iInd)
    jIndices = jInd ++ jInd

-- All rows, columns and diagonals (len> = 4) on board
allLists :: Board -> [[Int]]
allLists board = board ++ (transpose board) ++ (getDiagonals board)

-- Do collected four in a row, 0 - no collected four
hasConnectedFour :: [Int] -> Player
hasConnectedFour xs 
  | any (== [1,1,1,1]) grouped = 1
  | any (== [2,2,2,2]) grouped = 2
  | otherwise = 0
  where
    grouped = group xs

-- Won anyone, 0 - no one wins
isWin :: Board -> Player
isWin board = fromMaybe 0 $ find (/= 0) $ map hasConnectedFour (allLists board)

-- Splitting a list into sublists by 4 elements
divideToFour :: [Int] -> [[Int]]
divideToFour [_,_,_] = []
divideToFour xs = [take 4 xs] ++ (divideToFour (drop 1 xs))

-- Is there an open triple in a row, 0 - no open triples
hasOpenThree :: [Int] -> Player
hasOpenThree xs
  | delete 0 xs == [1,1,1] = 1
  | delete 0 xs == [2,2,2] = 2
  | otherwise = 0

-- The number of open threes for each player
countThrees :: Board -> (Int, Int)
countThrees board = foldl (\(acc1, acc2) x -> case x of 
                                            1 -> (acc1 + 1, acc2)
                                            2 -> (acc1, acc2 + 1)
                                            0 -> (acc1, acc2)) (0, 0) list
  where
    list = map hasOpenThree $ concatMap divideToFour (allLists board)

heuristics :: Board -> Int
heuristics board
  | p == 1 = 10000
  | p == 2 = -10000
  | otherwise = c1 - c2 
  where
    p = isWin board
    (c1, c2) = countThrees board

-- Compare boards on heuristics
compareBoards :: Board -> Board -> Ordering
compareBoards b1 b2 = compare (heuristics b1) (heuristics b2)

-- Choosing the best move
makeMove :: Board -> Board
makeMove board = if (heuristics maxFirstMove == 10000) then maxFirstMove
                 else case index of
                        Nothing -> []
                        Just i -> firstMove !! i
  where
    firstMove = possibleMoves 1 board
    maxFirstMove = maximumBy compareBoards firstMove
    secondMove = map (minimumBy compareBoards) $ map (possibleMoves 2) firstMove 
    index = elemIndex (maximumBy compareBoards secondMove) secondMove
