module Board where

import Data.List
import Data.Maybe

type Board = [[Int]]

type Player = Int

-- Игрок ставит фишку на позицию i j
makeMove :: Board -> Player -> Int -> Int -> Board
makeMove b p i j = fst $ foldl (\(acc, k) x -> if (k == i) then (acc++[newRow x], k+1) 
                                               else (acc ++ [x], k+1)) ([], 0) b
  where
    newRow row = fst $ foldl (\(acc, k) x -> if (k == j) then (acc ++ [p], k+1) 
                                             else (acc ++ [x], k+1)) ([], 0) row

-- Все возможные ходы игрока
possibleMoves :: Board -> Player -> [Board]
possibleMoves b p = filter (not . null) $ zipWith 
                    (\i j -> if i == -1 then [] else makeMove b p i j) indices [0..6]
  where
    indices = zipWith findEmptyI (replicate 7 b) [0..6]
    findEmptyI b j = fst $ foldr (\x (i, f) -> 
                      if f then (if x !! j == 0 then (i, False) else (i-1, True)) 
                      else (i, f)) (5, True) b

-- Можно ли сделать какой-либо ход
notDead :: Board -> Bool
notDead b = or $ map (\x -> any (== 0) x) b 

-- Получаем все диагонали
getDiagonals :: Board -> [[Int]]
getDiagonals b = zipWith diagonal iIndices jIndices
  where
    diagonal is js = zipWith (\i j -> (b !! i) !! j) is js
    iInd = [[2,3,4,5], [1,2,3,4,5], [0,1,2,3,4,5], [0,1,2,3,4,5], [0,1,2,3,4], [0,1,2,3]]
    jInd = [[0,1,2,3], [0,1,2,3,4], [0,1,2,3,4,5], [1,2,3,4,5,6], [2,3,4,5,6], [3,4,5,6]]
    iIndices = iInd ++ (map reverse $ reverse iInd)
    jIndices = jInd ++ jInd

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
isWin b = fromMaybe 0 $ find (/= 0) $ map hasConnectedFour allLists
  where
    allLists = b ++ (transpose b) ++ (getDiagonals b)

-- Количество открытых троек для каждого игрока
countPieces :: Board -> (Int, Int)
countPieces = undefined

divideToFour :: [Int] -> [[Int]]
divideToFour [_,_,_] = []
divideToFour xs = [take 4 xs] ++ (divideToFour (drop 1 xs))

test = [[2,0,0,0,0,0,0], 
        [2,0,0,0,1,0,0], 
        [1,0,0,2,0,0,0], 
        [2,0,1,0,0,0,0], 
        [2,1,0,0,0,0,0], 
        [2,0,0,2,2,1,2]] :: Board
