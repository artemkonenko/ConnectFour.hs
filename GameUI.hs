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
  Start   -> [button (0,-100) blue "Press 1 to start game with player",
              button (0, 100) blue "Press 2 to start game with AI"]
  --(renderMessage "Press any 1-6 key to start.") ++ [button BTN_Normal (-100, 0) blue "Test"]
  Game    -> renderBoard ++ renderTestMessage
  WinEnd  -> renderBoard ++ (renderMessage ("Player " ++ (show $ currentPlayer state) ++ " win!"))
  FailEnd -> (renderMessage "Any player defeat!")
  where
    renderBoard = concat (map (\(row, y) -> map (\(colour, x) -> stateToForm x y colour) row) (enumBoard $ board state))
    renderTestMessage = [move (100, 200) $ toForm $ Text.plainText $ "You shoud win"]
    renderMessage message = [messageBox (-25,-100) message]

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

winEndState :: Player -> Board -> State
winEndState player board = State { gameState = WinEnd,
                          currentPlayer = player,
                          keyboardBlock = True,
                          board = board,
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

newAIBlockedState :: State -> State
newAIBlockedState (State { gameState = gameState,
                     currentPlayer = player,
                     board = board,
                     keyboardBlock = keyboardBlock,
                     gameType = gameType }) = State { gameState = gameState,
                              currentPlayer = (mod player 2) + 1,
                              board = makeMove board,
                              keyboardBlock = False,
                              gameType = gameType}

brandnewState :: GameType -> State
brandnewState gameType = State { gameState = Game,
                        currentPlayer = 1,
                        keyboardBlock = False,
                        board = replicate boardHeight $ replicate boardWidth 0,
                        gameType = gameType}

---------------------- UI elements
--data ButtonType = BTN_Normal | BTN_Hovered | BTN_Pressed

button :: (Int, Int) -> Color -> String -> Form
button (x, y) color msg = move (fromIntegral x, fromIntegral y) $ group [base, shadow, message]
  where
    base = filled color $ rect (40 + 11 * (fromIntegral $ length msg)) 80
    shadow = move (-2, -2) base
    message = toForm $ Text.text $ textFormat $ msg
    textFormat = (Text.color $ yellow) . Text.bold . Text.toText

messageBox :: (Int, Int)-> String -> Form
messageBox (x, y) msg = move (fromIntegral x, fromIntegral y) $ group [base, shadow, message]
  where
    base = filled purple $ rect (250 + 11 * (fromIntegral $ length msg)) 100
    shadow = move (-2, -2) base
    message = toForm $ Text.text $ textFormat $ msg
    textFormat = (Text.color $ white) . Text.bold . Text.header . Text.toText

--button btntype (x, y) color massage = move (fromIntegral x, fromIntegral y) $ filled color $ RectangleShape (200, 350)
