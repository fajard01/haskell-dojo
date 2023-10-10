module TTT.A1 where

import Data.Char (toUpper)

-- Q#01
_SIZE_ :: Int
_SIZE_ = 3

-- Q#02
_DISPLAY_LOGO_ :: Bool
_DISPLAY_LOGO_ = True

-- Q#03
convertRowIndex :: Char -> Int
convertRowIndex char = fromEnum (toUpper char) - 65

-- Q#04
_INVALID_MOVE_ :: (Int,Int)
_INVALID_MOVE_ = (-1,-1)

-- Q#05
_SEP_ :: String
_SEP_ = "_|_"

-- Q#06
-- E == Empty
data Square = O | E | X 
                deriving (Show, Eq) 

-- Q#07
data GameState = X_Won | O_Won | Tie | Progress
                    deriving (Show, Eq)
                    
-- Q#08
type Player = Square
type Row    = [Square]
type Line   = [Square]
type Board  = [Row]
type Move   = (Int,Int)

-- Q#09
getFirstPlayer :: Bool -> Player
getFirstPlayer bool = if bool
                            then X
                            else O

getFirstPlayer_ :: Bool -> Player
getFirstPlayer_ bool | bool         = X
                     | otherwise = O

-- Q#10
showGameState :: GameState -> String
showGameState gamestate = case gamestate of 
                                X_Won     -> "X player wins!"
                                O_Won     -> "O Player wins!"
                                Tie       -> "Game is a tie."
                                Progress  -> "Game is still in progress." 

-- Q#11
switchPlayer :: Player -> Player
switchPlayer player | player == X = O
                    | player == O = X 
                    | otherwise   = E
                
-- Q#12
showSquare :: Square -> String
showSquare square | square == X = "X"
                  | square == O = "O"
                  | otherwise   = "_"