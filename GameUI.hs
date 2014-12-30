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
windowHeight= 700 :: Int

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
coordToForm x y = move ((fromIntegral x) * 90 - 275, (fromIntegral y) * 90 - 200)

stateToForm :: Int -> Int -> Player -> Form
stateToForm x y player = coordToForm x y $ filled (playerToColor player) $ circle 40

stateToRenderlist :: (Int, Int) -> State -> [Form]
stateToRenderlist (w,h) state = case gameState state of
  Start   -> [button (0,-100) purple "Press 1 to start game with player",
              button (0, 100) purple "Press 2 to start game with AI"]
  --(renderMessage "Press any 1-6 key to start.") ++ [button BTN_Normal (-100, 0) blue "Test"]
  Game    -> renderBoard ++ renderTestMessage
  WinEnd  -> renderBoard ++ (renderMessage ( playerToTextlocor ++ " player win!"))
  FailEnd -> renderBoard ++ (renderMessage "Any player defeat!")
  where
    renderBoard = background ++ (concat (map (\(row, y) -> map (\(colour, x) -> stateToForm x y colour) row) (enumBoard $ board state)))
    renderTestMessage = [move (-5, 320) $ toForm $ Text.text $ textFormat $ playerToTextlocor ++ "'s turn"]
    renderMessage message = [messageBox (-5, -100) message]
    background = [move (-355, -360) $ toForm $ image 700 700 "background.png"]
    textFormat = (Text.color $ white) . Text.bold . Text.toText
    playerToTextlocor = (if currentPlayer state == 1 then "Red" else "Yellow")

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

failEndState :: Board -> State
failEndState board = State { gameState = FailEnd,
                          currentPlayer = 0,
                          keyboardBlock = True,
                          board = board,
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
newBlockedState state row col = case gameType state of
                                  VsUser  -> checkBoard $ userStep state 
                                  VsAI    -> case gameState . checkBoard $ userStep state of
                                                Game -> checkBoard . aiStep $ userStep state
                                                otherwise -> checkBoard $ userStep state
  where
    userStep (State { gameState = gameState,
                     currentPlayer = player,
                     board = board,
                     keyboardBlock = keyboardBlock,
                     gameType = gameType }) = State { gameState = gameState,
                                                      currentPlayer = (mod player 2) + 1,
                                                      board = setChip col row player board,
                                                      keyboardBlock = True,
                                                      gameType = gameType}
    aiStep (State { gameState = gameState,
                     currentPlayer = player,
                     board = board,
                     keyboardBlock = keyboardBlock,
                     gameType = gameType }) = State { gameState = gameState,
                                                      currentPlayer = (mod player 2) + 1,
                                                      board = makeMove board,
                                                      keyboardBlock = keyboardBlock,
                                                      gameType = gameType}
    checkBoard :: State -> State -- check, that game still continue
    checkBoard state =
      case isWin (board state) of
        1 -> winEndState 1 (board state)
        2 -> winEndState 2 (board state)
        otherwise ->  if notDead (board state) then
                        state
                      else
                        failEndState (board state)

brandnewState :: GameType -> State
brandnewState gameType = State { gameState = Game,
                        currentPlayer = 2,
                        keyboardBlock = False,
                        board = replicate boardHeight $ replicate boardWidth 0,
                        gameType = gameType}

---------------------- UI elements
button :: (Int, Int) -> Color -> String -> Form
button (x, y) color msg = move (fromIntegral x, fromIntegral y) $ group [base, shadow, message]
  where
    base = filled color $ rect (40 + 11 * (fromIntegral $ length msg)) 80
    shadow = move (-2, -2) base
    message = toForm $ Text.text $ textFormat $ msg
   -- textFormat = (Text.color $ yellow) . Text.bold . Text.toText
    textFormat = (Text.color $ white) . Text.bold . Text.toText

messageBox :: (Int, Int)-> String -> Form
messageBox (x, y) msg = move (fromIntegral x, fromIntegral y) $ group [base, shadow, message]
  where
    base = filled purple $ rect (250 + 11 * (fromIntegral $ length msg)) 100
    shadow = move (-2, -2) base
    message = toForm $ Text.text $ textFormat $ msg
    textFormat = (Text.color $ white) . Text.bold . Text.header . Text.toText

--button btntype (x, y) color massage = move (fromIntegral x, fromIntegral y) $ filled color $ RectangleShape (200, 350)
