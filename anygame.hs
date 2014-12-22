import FRP.Helm
import FRP.Helm.Utilities
import qualified FRP.Helm.Keyboard as Keyboard
import qualified FRP.Helm.Window as Window
import Data.List (elemIndex,findIndex)

boardWidth = 7
boardHeight = 6

--type Board [[Int]]
{- 6x7 circles
0 - empty
1 - red
2 - yellow
-}
data State = State { currentColor :: Int,
                     board :: [[Int]],
                     keyboardBlock :: Bool --because we should filter the noise signals
                   }

setChip :: Int -> Int -> [[Int]] -> [[Int]]
setChip col curColor board = map (\(row, y) -> map (\(colour, x) -> chipMap x y colour) row) enumBoard
  where
    enumBoard = zip (map (\x -> zip x [0..]) board) [0..]
    targetRow = findIndex (/=0) $ map (!!col) board
    chipMap x y colour = case targetRow of
      Nothing -> if (x == col && y == boardHeight - 1) then curColor else colour
      Just r  
          | r > 0 -> if (x == col && y == r - 1) then curColor else colour
          | otherwise -> colour
    
step :: Maybe Int -> State -> State
step Nothing (State { currentColor = currentColor, board = board, keyboardBlock = keyboardBlock }) =
  State { currentColor = currentColor,
          board = board,
          keyboardBlock = False}
step (Just col) (State { currentColor = currentColor, board = board, keyboardBlock = keyboardBlock }) =
  if keyboardBlock then
    State { currentColor = currentColor,
          board = board,
          keyboardBlock = True}
  else
    State { currentColor = (mod currentColor 2) + 1,
            board = setChip col currentColor board,
            keyboardBlock = True}

stateColor :: Int -> Color
stateColor 0 = white
stateColor 1 = red
stateColor 2 = yellow

coordToForm :: Int -> Int -> (Form -> Form)
coordToForm x y = move ((fromIntegral x) * 90 - 300, (fromIntegral y) * 90 - 250)

stateToForm :: Int -> Int -> Int -> Form
stateToForm x y color = coordToForm x y $ filled (stateColor color) $ circle 40

stateToRenderlist :: (Int, Int) -> State -> [Form]
stateToRenderlist (w,h) (State { currentColor = currentColor, board = board }) =
  concat (map (\(row, y) -> map (\(colour, x) -> stateToForm x y colour) row) enumBoard)
  where
    enumBoard = zip (map (\x -> zip x [0..]) board) [0..]

render :: (Int, Int) -> State -> Element
render (w, h) (State { currentColor = currentColor, board = board, keyboardBlock = keyboardBlock }) =
  centeredCollage w h ((stateToRenderlist (w,h) (State { currentColor = currentColor,
                                                         board = board,
                                                         keyboardBlock = keyboardBlock }))) 

main :: IO ()
main = run defaultConfig $ render <~ Window.dimensions ~~ stepper
  where
    state = State { currentColor = 1,
                    keyboardBlock = False,
                    board = [[0,0,0,0,0,0,0],
                             [0,0,0,0,0,0,0],
                             [0,0,0,0,0,0,0],
                             [0,0,0,0,0,0,0],
                             [0,0,0,2,0,0,0],
                             [0,1,0,1,0,2,0]]}
    stepper = foldp step state (lift (elemIndex True) (combine [Keyboard.isDown Keyboard.Number1Key,
                                          Keyboard.isDown Keyboard.Number2Key,
                                          Keyboard.isDown Keyboard.Number3Key,
                                          Keyboard.isDown Keyboard.Number4Key,
                                          Keyboard.isDown Keyboard.Number5Key,
                                          Keyboard.isDown Keyboard.Number6Key,
                                          Keyboard.isDown Keyboard.Number7Key]))

