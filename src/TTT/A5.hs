module TTT.A5 where

import Control.Monad (when)
import System.Random.Stateful (globalStdGen, uniformM)
import TTT.A1
import TTT.A2
import TTT.A3
import TTT.A4
import TTT.Minimax
import Foreign (toBool)
import HM.Provided (_SPACE_)

-- Q#01
printBoard :: Board -> IO ()
printBoard board = putStrLn $ formatBoard board

-- Q#02
_LOGO_PATH_ :: FilePath
_LOGO_PATH_ = "./assets/ttt-logo.txt"

printLogo :: IO ()
printLogo = readFile _LOGO_PATH_ >>= 
            putStrLn

-- Q#03
_RANDOM_BOOL_ :: IO Bool
_RANDOM_BOOL_ = uniformM globalStdGen

firstPlayer :: IO Player
firstPlayer = _RANDOM_BOOL_ >>= 
              (\bool -> return $ getFirstPlayer bool)

-- Q#04
getMove :: Board -> IO Move
getMove board = getLine >>=
                (\line -> if isValidMove board $ stringToMove line
                              then return $ stringToMove line
                              else putStrLn "Invalid move! Try again" >>
                                   getMove board)
                                   
-- Q#05
play :: Board -> Player -> IO ()
play board player = 
     when _DISPLAY_LOGO_ printLogo >>
     _SPACE_ >>     
     play' board player
          where
               play' :: Board -> Player -> IO ()
               play' board' player' = 
                    printBoard board' >>
                    putStrLn (promptPlayer player') >> 
                    getMove board' >>=
                    (\move -> processMove move) >>=
                    (\(mvstate,mvboard) -> updateGame (mvstate,mvboard))
                         where
                              processMove :: Move -> IO (GameState, Board)
                              processMove move'
                                   | isValidMove board' move' =
                                        return $ playMove player' board' move' 
                                   | otherwise =
                                        putStrLn "Move invalid. Try again." >>
                                        return (getGameState board', board')      

                              updateGame :: (GameState, Board) -> IO () 
                              updateGame (newState, newBoard) 
                                   | newState == In_Progress = continueGame
                                   | otherwise               = endGame
                                        where 
                                             continueGame :: IO ()
                                             continueGame =
                                                  _SPACE_ >>
                                                  putStrLn (showGameState newState) >>
                                                  _SPACE_ >>
                                                  play' newBoard (switchPlayer player')
                                             endGame :: IO ()
                                             endGame =
                                                  printBoard newBoard >>
                                                  putStrLn (showGameState newState) >>
                                                  _SPACE_

-- Q#06

runTTT :: IO ()
runTTT = --putStrLn "Not implemented... yet!"
     firstPlayer >>= playEmptyBoard 
               where
                    playEmptyBoard = play _EMPTY_BOARD_

-- Q#07
printLogoDo :: IO ()
printLogoDo = do
     logo <- readFile _LOGO_PATH_
     putStrLn logo

-- Q#08
firstPlayerDo :: IO Player
firstPlayerDo = do
     bool <- _RANDOM_BOOL_
     return $ getFirstPlayer bool

-- Q#09Board -> IO Move
getMoveDo :: Board -> IO Move 
getMoveDo board = do 
     line <- getLine
     let move = stringToMove line
     if isValidMove board move
          then do 
               return move 
          else do 
               putStrLn "Invalid move! Try again"
               getMove board

-- Q#10
playDo :: Board -> Player -> IO ()
playDo board player = do
     when _DISPLAY_LOGO_ printLogo
     _SPACE_
     playDo' board player
          where
               playDo' :: Board -> Player -> IO ()
               playDo' board' player' = do 
                    printBoard board'
                    putStrLn $ promptPlayer player' 
                    move <- getMove board'
                    (mvstate, mvboard) <- processMove move
                    updateGame (mvstate, mvboard)
                         where
                              processMove :: Move -> IO (GameState, Board)
                              processMove move'
                                   | isValidMove board' move' =
                                        return $ playMove player' board' move' 
                                   | otherwise =
                                        putStrLn "Move invalid. Try again." >>
                                        return (getGameState board', board')      
                                               
                              updateGame :: (GameState, Board) -> IO () 
                              updateGame (newState, newBoard) 
                                   | newState == In_Progress = do continueGame
                                   | otherwise               = do endGame
                                        where 
                                             continueGame :: IO ()
                                             continueGame = do
                                                  _SPACE_
                                                  putStrLn $ showGameState newState
                                                  _SPACE_
                                                  playDo' newBoard (switchPlayer player')
                                             endGame :: IO ()
                                             endGame = do
                                                  printBoard newBoard
                                                  putStrLn $ showGameState newState 
                                                  _SPACE_

-- Extra: Implementation to play TicTacToe vs Computer
playComputer :: Board -> Player -> IO ()
playComputer board player = do
     when _DISPLAY_LOGO_ printLogo
     _SPACE_
     putStrLn "You are playing against the computer"
     putStrLn "You will start as player X while computer plays as player O"
     putStrLn "Good luck!"
     _SPACE_
     playComputer' board player
          where
               playComputer' :: Board -> Player -> IO ()
               playComputer' board' player' 
                    | player' == X = 
                         do printBoard board'
                            putStrLn $ promptPlayer player' 
                            move <- getMove board'
                            (mvstate, mvboard) <- processMove move
                            updateGame (mvstate, mvboard)
                    | player' == O =
                         do printBoard board'
                            putStrLn "Calculating best move ..." 
                            (mvstate, mvboard) <- playComputerMove player' board'
                            updateGame (mvstate, mvboard)
                                   where
                                        processMove :: Move -> IO (GameState, Board)
                                        processMove move'
                                              | isValidMove board' move' =
                                                       return $ playMove player' board' move' 
                                             | otherwise =
                                                       putStrLn "Move invalid. Try again." >>
                                                       return (getGameState board', board')      
                                               
                                        updateGame :: (GameState, Board) -> IO () 
                                        updateGame (newState, newBoard) 
                                             | newState == In_Progress = do continueGame
                                             | otherwise               = do endGame
                                                  where 
                                                       continueGame :: IO ()
                                                       continueGame = do
                                                            _SPACE_
                                                            putStrLn $ showGameState newState
                                                            _SPACE_
                                                            playComputer' newBoard (switchPlayer player')
                                                       endGame :: IO ()
                                                       endGame = do
                                                            printBoard newBoard
                                                            putStrLn $ showGameState newState 
                                                            _SPACE_

runTTTvsComputer :: IO ()
runTTTvsComputer = playComputer _EMPTY_BOARD_ X
