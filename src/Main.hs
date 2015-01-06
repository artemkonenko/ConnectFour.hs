import FRP.Helm
import qualified FRP.Helm.Keyboard as Keyboard
import qualified FRP.Helm.Window as Window
import Data.List (elemIndex)
import Board
import GameUI
    
step :: Maybe Int -> State -> State
step Nothing state = unblockState state
step (Just col) state = case gameState state of
    Start   ->  selectGame
    Game    ->  if (keyboardBlock state) then
                unblockState state
              else
                  case freeRow col (board state) of
                    Nothing  -> blockState state
                    Just row -> newBlockedState state row col
    WinEnd  -> selectGame
    FailEnd -> selectGame
  where
    selectGame = case col of
                  0 -> brandnewState VsUser
                  1 -> brandnewState VsAI
                  _ -> state
    
main :: IO ()
main = run engineConfig $ render <~ Window.dimensions ~~ stepper
  where
    state = State { gameState = Start,
                    currentPlayer = 0,
                    gameType = VsAI,
                    keyboardBlock = False,
                    board = []}
    stepper = foldp step state (lift (elemIndex True) keyboardkeys)
    keyboardkeys = (combine [Keyboard.isDown Keyboard.Number1Key,
                              Keyboard.isDown Keyboard.Number2Key,
                              Keyboard.isDown Keyboard.Number3Key,
                              Keyboard.isDown Keyboard.Number4Key,
                              Keyboard.isDown Keyboard.Number5Key,
                              Keyboard.isDown Keyboard.Number6Key,
                              Keyboard.isDown Keyboard.Number7Key])

