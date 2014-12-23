import FRP.Helm
import FRP.Helm.Utilities
import qualified FRP.Helm.Keyboard as Keyboard
import qualified FRP.Helm.Window as Window
import Data.List (elemIndex,findIndex)
import Board
import GameUI
    
step :: Maybe Int -> State -> State
step Nothing (State { gameState = gameState,
                      currentPlayer = player,
                      board = board,
                      keyboardBlock = keyboardBlock }) =
  State { gameState = gameState,
          currentPlayer = player,
          board = board,
          keyboardBlock = False}
          
step (Just col) (State { gameState = gameState,
                         currentPlayer = player,
                         board = board,
                         keyboardBlock = keyboardBlock }) =
  if keyboardBlock then
    if notDead board then
      oldBlockedState
    else
      finishState
  else
    case gameState of
      0 -> brandnewState
      1 -> case freeRow col board of
            Nothing  -> oldBlockedState
            Just row -> newBlockedState row
      2 -> finishState
  where
    oldBlockedState = State { gameState = gameState,
                              currentPlayer = player,
                              board = board,
                              keyboardBlock = True}
    newBlockedState row = State { gameState = gameState,
                                  currentPlayer = (mod player 2) + 1,
                                  board = setChip col row player board,
                                  keyboardBlock = True}
    brandnewState = State { gameState = 1,
                            currentPlayer = 1,
                            keyboardBlock = False,
                            board = replicate boardHeight $ replicate boardWidth 0}
    finishState = State { gameState = 2,
                          currentPlayer = player,
                          keyboardBlock = keyboardBlock,
                          board = board}
    
render :: (Int, Int) -> State -> Element
render (w, h) state = centeredCollage w h (stateToRenderlist (w,h) state) 

main :: IO ()
main = run defaultConfig $ render <~ Window.dimensions ~~ stepper
  where
    state = State { gameState = 0,
                    currentPlayer = 1,
                    keyboardBlock = False,
                    board = []}
    stepper = foldp step state (lift (elemIndex True) (combine [Keyboard.isDown Keyboard.Number1Key,
                                          Keyboard.isDown Keyboard.Number2Key,
                                          Keyboard.isDown Keyboard.Number3Key,
                                          Keyboard.isDown Keyboard.Number4Key,
                                          Keyboard.isDown Keyboard.Number5Key,
                                          Keyboard.isDown Keyboard.Number6Key,
                                          Keyboard.isDown Keyboard.Number7Key]))

