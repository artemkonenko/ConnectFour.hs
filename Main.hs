import FRP.Helm
import FRP.Helm.Utilities
import qualified FRP.Helm.Keyboard as Keyboard
import qualified FRP.Helm.Window as Window
import Data.List (elemIndex,findIndex)
import Board
import GameUI
    
step :: Maybe Int -> State -> State
step Nothing state = case gameState state of
  Start   -> state
  Game    -> checkBoard state
  WinEnd  -> state 
  FailEnd -> state 
  where
    checkBoard :: State -> State -- check, that game still continue
    checkBoard state =
      case isWin (board state) of
        1 -> winEndState 1
        2 -> winEndState 2
        otherwise ->  if notDead (board state) then
                        unblockState state
                      else
                        failEndState
step (Just col) state = case gameState state of
  Start   ->  brandnewState (gameType state)
  Game    ->  if (keyboardBlock state) then
              unblockState state
            else
                case freeRow col (board state) of
                  Nothing  -> blockState state
                  Just row -> newBlockedState state row col
  WinEnd  -> state
  FailEnd -> state
    
main :: IO ()
main = run engineConfig $ render <~ Window.dimensions ~~ stepper
  where
    state = State { gameState = Start,
                    currentPlayer = 1,
                    gameType = VsAI,
                    keyboardBlock = False,
                    board = []}
    stepper = foldp step state (lift (elemIndex True) (combine [Keyboard.isDown Keyboard.Number1Key,
                                          Keyboard.isDown Keyboard.Number2Key,
                                          Keyboard.isDown Keyboard.Number3Key,
                                          Keyboard.isDown Keyboard.Number4Key,
                                          Keyboard.isDown Keyboard.Number5Key,
                                          Keyboard.isDown Keyboard.Number6Key,
                                          Keyboard.isDown Keyboard.Number7Key]))