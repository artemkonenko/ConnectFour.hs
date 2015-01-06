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

windowWidth :: Int
windowWidth = 700

windowHeight :: Int
windowHeight = 700

engineConfig :: EngineConfig
engineConfig = EngineConfig { windowDimensions = (windowWidth, windowHeight),
                              windowIsFullscreen = False,
                              windowIsResizable = True,
                              windowTitle = "Connect Four.hs"
                            }

render :: (Int, Int) -> State -> Element
render (w, h) state = centeredCollage w h (renderState state) 

playerToColor :: Player -> Color
playerToColor 0 = white
playerToColor 1 = red
playerToColor 2 = yellow

coordToForm :: Int -> Int -> (Form -> Form)
coordToForm x y = move ((fromIntegral x) * 90 - 270, (fromIntegral y) * 90 - 190)

stateToForm :: Int -> Int -> Player -> Form
stateToForm x y player = coordToForm x y $ filled (playerToColor player) $ circle 40

renderState :: State -> [Form]
renderState state = background playerColor : case gameState state of
  Start   -> [selectGameButtons]
  Game    -> [renderBoard]
  WinEnd  -> [renderBoard, renderMessage ( playerToTextlocor ++ " player win!"), moveY (40) selectGameButtons]
  FailEnd -> [renderBoard, renderMessage "Any player defeat!", moveY (40) selectGameButtons]
  where
    renderBoard = group $ (concat (map (\(row, y) -> map (\(colour, x) -> stateToForm x y colour) row) (enumBoard $ board state)))
    renderMessage message = messageBox black playerColor (0,-100) message
    background color = move (-350, -350) $ toForm $ image 700 700 $ if color == red then
                                                                      "img/backgroundred.png"
                                                                    else
                                                                      "img/backgroundyellow.png"
    playerToTextlocor = (if currentPlayer state == 1 then "Red" else "Yellow")
    selectGameButtons = group [btns (0, 0)   "Press 1 to start game with player",
                               btns (0, 100) "Press 2 to start game with AI"]
    btns = button black playerColor
    playerColor = playerToColor $ currentPlayer state

winEndState :: Player -> Board -> State
winEndState player newboard = State {gameState = WinEnd,
                                  currentPlayer = player,
                                  keyboardBlock = True,
                                  board = newboard,
                                  gameType = VsUser}

failEndState :: Board -> State
failEndState newboard = State {gameState = FailEnd,
                            currentPlayer = 0,
                            keyboardBlock = True,
                            board = newboard,
                            gameType = VsUser}

blockState :: State -> State
blockState state  = state { keyboardBlock = True}

unblockState :: State -> State
unblockState state = state { keyboardBlock = False }

newBlockedState :: State -> Int -> Int -> State
newBlockedState state row col = case gameType state of
                                  VsUser  -> checkBoard $ userStep 
                                  VsAI    -> case gameState . checkBoard $ userStep of
                                                Game -> checkBoard . aiStep $ userStep
                                                _    -> checkBoard $ userStep
  where
    userStep = state{ currentPlayer = (mod (currentPlayer state) 2) + 1,
                            board = setChip col row (currentPlayer state) (board state),
                            keyboardBlock = True }
    aiStep stateAfterStep = stateAfterStep { currentPlayer = (mod (currentPlayer state) 2) + 1,
                                             board = makeMove (board state)}
    checkBoard :: State -> State -- check, that game still continue
    checkBoard stateAfterStep =
      case isWin (board stateAfterStep) of
        1 ->  winEndState 1 (board stateAfterStep)
        2 ->  winEndState 2 (board stateAfterStep)
        _ ->  if notDead (board stateAfterStep) then
                stateAfterStep
              else
                failEndState (board stateAfterStep)

brandnewState :: GameType -> State
brandnewState newGameType = State {gameState = Game,
                                currentPlayer = 2,
                                keyboardBlock = False,
                                board = replicate boardHeight $ replicate boardWidth 0,
                                gameType = newGameType}

---------------------- UI elements
button :: Color -> Color -> (Int, Int) -> String -> Form
button bgcolor txtcolor (x, y) msg = move (fromIntegral x, fromIntegral y) $ group [base, message]
  where
    curvone = filled bgcolor $ circle 40
    curvs = group [move ((11 * (fromIntegral $ length msg)) / 2,  0) curvone
                  ,move (-(11 * (fromIntegral $ length msg)) / 2, 0) curvone]
    base = group [curvs, filled bgcolor $ rect (11 * (fromIntegral $ length msg)) 80]
    message = toForm $ Text.text $ textFormat $ msg
    textFormat = (Text.color $ txtcolor) . Text.bold . Text.toText

messageBox :: Color -> Color -> (Int, Int)-> String -> Form
messageBox bgcolor txtcolor (x, y) msg = move (fromIntegral x, fromIntegral y) $ group [base, shadow, message]
  where
    curvone = filled bgcolor $ circle 50
    curvs = group [move ((22 * (fromIntegral $ length msg)) / 2,  0) curvone
                  ,move ((-22 * (fromIntegral $ length msg)) / 2, 0) curvone]
    base = group [curvs, filled bgcolor $ rect (22 * (fromIntegral $ length msg)) 100]
    shadow = move (-2, -2) base
    message = toForm $ Text.text $ textFormat $ msg
    textFormat = (Text.color $ txtcolor) . Text.bold . Text.header . Text.toText
