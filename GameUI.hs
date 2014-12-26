module GameUI where

import FRP.Helm
import qualified FRP.Helm.Text as Text
import Board

data GameState = Start | Game | WinEnd | FailEnd
data GameType = VsUser | VsAI

data State = State { gameState :: GameState
                   , currentPlayer :: Player
                   , board :: Board
                   , keyboardBlock :: Bool --because we should filter the noise signals
                   , gameType :: GameType
                   }

-- We need some getters
--gameState :: State -> GameState
--gameState (State {gameState = gameState}) = gameState

windowWidth = 800 :: Int
windowHeight= 600 :: Int

engineConfig :: EngineConfig
engineConfig = EngineConfig { windowDimensions = (windowWidth, windowHeight),
                              windowIsFullscreen = False,
                              windowIsResizable = True,
                              windowTitle = "Connect Four.hs"
                            }

render :: (Int, Int) -> State -> Element
render (w, h) state = centeredCollage w h (stateToRenderlist (w,h) state) 

playerToColor :: Player -> Color
playerToColor 0 = white
playerToColor 1 = red
playerToColor 2 = yellow

coordToForm :: Int -> Int -> (Form -> Form)
coordToForm x y = move ((fromIntegral x) * 90 - 300, (fromIntegral y) * 90 - 250)

stateToForm :: Int -> Int -> Player -> Form
stateToForm x y player = coordToForm x y $ filled (playerToColor player) $ circle 40

stateToRenderlist :: (Int, Int) -> State -> [Form]
stateToRenderlist (w,h) state = case gameState state of
  Start   -> (renderMessage "Press any 1-6 key to start.")
  Game    -> renderBoard ++ renderTestMessage
  WinEnd  -> (renderMessage ("Player " ++ (show $ currentPlayer state) ++ " win!"))
  FailEnd -> (renderMessage "Any player defeat!")
  where
    renderBoard = concat (map (\(row, y) -> map (\(colour, x) -> stateToForm x y colour) row) (enumBoard $ board state))
    renderTestMessage = [move (100, 200) $ toForm $ Text.plainText $ "You shoud win"]
    renderMessage message = [move (0, -100) $ toForm $ Text.text $ textFormat $ message ]
    textFormat = (Text.color $ textColor) . Text.bold . Text.header . Text.toText
    textColor = green

unblockState :: State -> State
unblockState (State { gameState = gameState,
                     currentPlayer = player,
                     board = board,
                     keyboardBlock = keyboardBlock,
                     gameType = gameType }) = State { gameState = gameState,
                              currentPlayer = player,
                              board = board,
                              keyboardBlock = False,
                              gameType = gameType}

winEndState :: Player -> State
winEndState player = State { gameState = WinEnd,
                          currentPlayer = player,
                          keyboardBlock = True,
                          board = emptyBoard,
                          gameType = VsUser}

failEndState :: State
failEndState = State { gameState = FailEnd,
                          currentPlayer = 0,
                          keyboardBlock = True,
                          board = emptyBoard,
                          gameType = VsUser}

blockState :: State -> State
blockState (State { gameState = gameState,
                     currentPlayer = player,
                     board = board,
                     keyboardBlock = keyboardBlock,
                     gameType = gameType }) = State { gameState = gameState,
                              currentPlayer = player,
                              board = board,
                              keyboardBlock = True,
                              gameType = gameType}

newBlockedState :: State -> Int -> Int -> State
newBlockedState (State { gameState = gameState,
                     currentPlayer = player,
                     board = board,
                     keyboardBlock = keyboardBlock,
                     gameType = gameType }) row col = State { gameState = gameState,
                              currentPlayer = (mod player 2) + 1,
                              board = setChip col row player board,
                              keyboardBlock = True,
                              gameType = gameType}

brandnewState :: GameType -> State
brandnewState gameType = State { gameState = Game,
                        currentPlayer = 1,
                        keyboardBlock = False,
                        board = replicate boardHeight $ replicate boardWidth 0,
                        gameType = gameType}