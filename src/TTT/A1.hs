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
convertRowIndex c = fromEnum (toUpper c) - 65

-- Q#04
_INVALID_MOVE_ :: (Int,Int)
_INVALID_MOVE_ = (-1,-1)

-- Q#05
_SEP_ :: String
_SEP_ = "_|_"

-- Q#06
data Square = O | X | B 
                deriving (Show, Eq) 

-- Q#07
data GameState = XWon | OWon | Tie | Prog
                    deriving (Show, Eq)
                    
-- Q#08
type Player = Square
type Row    = [Square]
type Line   = [Square]
type Board  = [Row]
type Move   = (Int,Int)

-- Q#09
getFirstPlayer :: Bool -> Player
getFirstPlayer b = if b
                        then X
                        else O

getFirstPlayer_ :: Bool -> Player
getFirstPlayer_ b | b         = X
                  | otherwise = O

-- Q#10
showGameState :: GameState -> String
showGameState gs = case gs of 
                        XWon -> "X player wins!"
                        OWon -> "O Player wins!"
                        Tie  -> "Game is a tie."
                        Prog -> "Game is still in progress." 

-- Q#11
switchPlayer :: Player -> Player
switchPlayer p | p == X    = O
               | p == O    = X 
               | otherwise = B
                
-- Q#12
showSquare :: Square -> String
showSquare s | s == X    = "X"
             | s == O    = "O"
             | otherwise = "_"