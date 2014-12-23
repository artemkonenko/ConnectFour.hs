module GameUI where

import FRP.Helm
import qualified FRP.Helm.Text as Text
import Board

playerToColor :: Player -> Color
playerToColor 0 = white
playerToColor 1 = red
playerToColor 2 = yellow

coordToForm :: Int -> Int -> (Form -> Form)
coordToForm x y = move ((fromIntegral x) * 90 - 300, (fromIntegral y) * 90 - 250)

stateToForm :: Int -> Int -> Player -> Form
stateToForm x y player = coordToForm x y $ filled (playerToColor player) $ circle 40

stateToRenderlist :: (Int, Int) -> State -> [Form]
stateToRenderlist (w,h) (State { gameState = gameState,
                                 board = board }) = case gameState of
  0 -> (renderMessage "Press any 1-6 key to start.")
  1 -> renderBoard ++ renderTestMessage
  2 -> (renderMessage "Any player wins!")
  where
    renderBoard = concat (map (\(row, y) -> map (\(colour, x) -> stateToForm x y colour) row) (enumBoard board))
    renderTestMessage = [move (100, 200) $ toForm $ Text.plainText $ "You shoud win"]
    renderMessage message = [move (0, -100) $ toForm $ Text.text $ textFormat $ message ]
    textFormat = (Text.color $ textColor) . Text.bold . Text.header . Text.toText
    textColor = green
