module Board where

import Data.List
import Data.Maybe

type Board = [[Int]]
type EnumeratedBoard = [([(Int, Int)], Int)]

boardWidth = 7 :: Int
boardHeight = 6 :: Int

type Player = Int -- 0: none, 1: red, 2: yellow

-- Enumerate board cells
enumBoard :: Board -> EnumeratedBoard
enumBoard board = zip (map (\x -> zip x [0..]) board) [0..]

-- Player set chip into (x, y). Yet another, but more readable. 
setChip :: Int -> Int -> Player -> Board -> Board
setChip col row curPlayer board = map (\(row, y) -> map (\(player, x) -> chipMap x y player) row) (enumBoard board)
  where
    chipMap x y player = if (x == col && y == row) then curPlayer else player

-- Index of free row in certain collumn
freeRow :: Int -> Board -> Maybe Int
freeRow col board = case findIndex (/=0) $ map (!!col) board of
                      Nothing -> Just (boardHeight - 1)
                      Just row
                        | row /= 0  -> Just (row - 1)
                        | otherwise -> Nothing

-- Все возможные ходы игрока
possibleMoves :: Player -> Board -> [Board]
possibleMoves player board = filter (not . null) $ zipWith 
         (\row j -> case row of
                      Just i -> setChip j i player board
                      Nothing -> []) indices [0..boardWidth-1]
  where
    indices = zipWith freeRow [0..boardWidth-1] (replicate boardWidth board)

-- Можно ли сделать какой-либо ход
notDead :: Board -> Bool
notDead board = or $ map (\x -> any (== 0) x) board 

-- Получаем все диагонали
getDiagonals :: Board -> [[Int]]
getDiagonals board = zipWith diagonal iIndices jIndices
  where
    diagonal is js = zipWith (\i j -> (board !! i) !! j) is js
    iInd = [[2,3,4,5], [1,2,3,4,5], [0,1,2,3,4,5], [0,1,2,3,4,5], [0,1,2,3,4], [0,1,2,3]]
    jInd = [[0,1,2,3], [0,1,2,3,4], [0,1,2,3,4,5], [1,2,3,4,5,6], [2,3,4,5,6], [3,4,5,6]]
    iIndices = iInd ++ (map reverse $ reverse iInd)
    jIndices = jInd ++ jInd

-- Все строки, столбцы и диагонали (len >= 4) доски
allLists :: Board -> [[Int]]
allLists board = board ++ (transpose board) ++ (getDiagonals board)

-- Есть ли собранная четверка в строке, 0 - нет собранной четверки
hasConnectedFour :: [Int] -> Player
hasConnectedFour xs 
  | any (== [1,1,1,1]) grouped = 1
  | any (== [2,2,2,2]) grouped = 2
  | otherwise = 0
  where
    grouped = group xs

-- Выиграл ли кто-нибудь, 0 - никто не выиграл
isWin :: Board -> Player
isWin board = fromMaybe 0 $ find (/= 0) $ map hasConnectedFour (allLists board)

-- Разбиваем список на подсписки по 4 элемента
divideToFour :: [Int] -> [[Int]]
divideToFour [_,_,_] = []
divideToFour xs = [take 4 xs] ++ (divideToFour (drop 1 xs))

-- Есть ли открытая тройка в строке, 0 - нет открытой тройки
hasOpenThree :: [Int] -> Player
hasOpenThree xs
  | delete 0 xs == [1,1,1] = 1
  | delete 0 xs == [2,2,2] = 2
  | otherwise = 0

-- Количество открытых троек для каждого игрока
countThrees :: Board -> (Int, Int)
countThrees board = foldl (\(acc1, acc2) x -> case x of 
                                            1 -> (acc1 + 1, acc2)
                                            2 -> (acc1, acc2 + 1)
                                            0 -> (acc1, acc2)) (0, 0) list
  where
    list = map hasOpenThree $ concatMap divideToFour (allLists board)

-- Эвристика
heuristic :: Board -> Int
heuristic board
  | isWin board == 1 = 10000
  | c2 >= 1 = -10000
  | otherwise = c1 
  where
    (c1, c2) = countThrees board

-- Сравниваем доски по эвристике
compareBoards :: Board -> Board -> Ordering
compareBoards b1 b2 = compare (heuristic b1) (heuristic b2)

-- Выбираем лучший ход
makeMove :: Board -> Board
makeMove board = case index of
                    Nothing -> []
                    Just i -> firstMove !! i
  where
    firstMove = possibleMoves 1 board
    secondMove = map (maximumBy compareBoards) $ map (possibleMoves 2) firstMove 
    index = elemIndex (maximumBy compareBoards secondMove) secondMove

test = [[0,0,0,0,0,0,0], 
        [0,0,0,0,0,0,0], 
        [0,0,0,0,0,0,0], 
        [0,0,0,0,0,2,0], 
        [0,0,0,0,0,2,0],  
        [1,0,1,0,0,2,0]] :: Board
