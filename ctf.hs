type Board = String

{-
Arg1: A list of boards representing the histor of the game
Arg2: A char indicating whether it's 'w' or 'b' turn to move
Arg3: An int indicating how many moves ahead the minimax algorithm is allowed to search
OUT: A board representing the next best move
-}
capture :: [Board] -> Char -> Int -> Board
capture (current:history) 'w' movesAhead = fst (minimax [current] movesAhead True history)
capture (current:history) 'b' movesAhead = (flipBoard (fst (minimax [flipBoard current] movesAhead True (flipEachBoard history))))


-----------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------MINIMAX ALGORITHM-----------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------
{-
Arg1: Some boards (the generated boards)
Arg2: More boards (the boards not allowed)
OUT: Arg1 minus (Arg1 intersection Arg2)
-}
filterBoards :: [Board] -> [Board] -> [Board]
filterBoards [] _ = []
filterBoards (b : bs) notAllowed
   | elem b notAllowed = filterBoards bs notAllowed
   | otherwise = b : filterBoards bs notAllowed

{-
Arg1: A list of board, goodness tuples
OUT: The tuple with the maximum value
-}
maxBoard :: [(Board, Float)] -> (Board, Float)
maxBoard tuples = maxBoardHelper tuples ("..", -100000)


{-
Arg1: A list of board, goodness tuples
Arg2: The maximum tuple so far
OUT: The tuple with the maximum value
-}
maxBoardHelper :: [(Board, Float)] -> (Board, Float) -> (Board, Float)
maxBoardHelper ((b, val) : []) (bmax, valmax)
   | val > valmax = (b, val)
   | otherwise = (bmax, valmax)

maxBoardHelper ((b, val) : tail) (bmax, valmax)
   | val > valmax = maxBoardHelper tail (b, val)
   | otherwise = maxBoardHelper tail (bmax, valmax)



{-
Arg1: A list of board, goodness tuples
OUT: The tuple with the minimum value
-}
minBoard :: [(Board, Float)] -> (Board, Float)
minBoard tuples = minBoardHelper tuples ("..", 100000)



{-
Arg1: A list of board, goodness tuples
Arg2: The minimum tuple so far
OUT: The tuple with the minimum value
-}
minBoardHelper :: [(Board, Float)] -> (Board, Float) -> (Board, Float)
minBoardHelper ((b, val) : []) (bmin, valmin)
   | val < valmin = (b, val)
   | otherwise = (bmin, valmin)

minBoardHelper ((b, val) : tail) (bmin, valmin)
   | val < valmin = minBoardHelper tail (bmin, valmin)
   | otherwise = minBoardHelper tail (bmin, valmin)


{-
Arg1: A list of boards
Arg2: height of the node
Arg3: True for max, False for min
Arg4: The previous boards, not allowed to be played again
OUT: The next best move, and its heuristic strength
-}
minimax :: [Board] -> Int -> Bool -> [Board] -> (Board, Float)
minimax (board : []) height minOrmax history
   | minOrmax == True && (filterBoards (generateNewBoards board) history) == [] = (board, goodness board)
   | minOrmax == False && (filterBoards (flipEachBoard (generateNewBoards (flipBoard board))) history) == [] = (board, goodness board)
   | height == 0 = (board, goodness board)
   | minOrmax == True = minimax (filterBoards (generateNewBoards board) history) (height - 1) False history
   | otherwise = minimax (filterBoards (flipEachBoard (generateNewBoards (flipBoard board))) history) (height - 1) True history

minimax (board : boards) height minOrmax history
   | minOrmax == True = maxBoard [(minimax [board] (height -1 ) False history), (minimax boards (height - 1) False history)]
   | otherwise = minBoard [(minimax [board] (height - 1) True history), (minimax boards (height - 1) True history)]


-----------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------MINIMAX ALGORITHM-----------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------



-----------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------STATIC BOARD EVALUATION---------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------
{-

----TODO----

Arg1: A board
Out: A number indicating how good of a board this is for 'w'
-}
goodness :: Board -> Float
goodness board = 0.0

-----------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------STATIC BOARD EVALUATION---------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------



-----------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------MOVE GENERATION-------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------



{-
Arg1: A board
Arg2: A position
OUT: A list of boards that can be generated from Arg1 by moving a white piece up
-}
generateNewWhiteMovesDown :: Board -> Int -> [Board]
generateNewWhiteMovesDown currBoard pos
   | pos > ((length currBoard) + (boardSize currBoard) - 1) = []
   | currBoard !! pos == 'w' && currBoard !! (pos + (boardSize currBoard)) == '-' = modifyBoard currBoard [('-', pos), ('w', pos + (boardSize currBoard))] : generateNewWhiteMovesDown currBoard (pos + 1)
   | otherwise = generateNewWhiteMovesDown currBoard (pos + 1)



{-
Arg1: A board
Arg2: A position
OUT: A list of boards that can be generated from Arg1 by moving a white piece left
-}
generateNewWhiteMovesLeft :: Board -> Int -> [Board]
generateNewWhiteMovesLeft currBoard pos
    | pos > (length currBoard) - 1 = []
    | mod (pos) (boardSize currBoard) == 0 = generateNewWhiteMovesLeft currBoard (pos + 1)
    | currBoard !! pos == 'w' && currBoard !! (pos - 1) == '-' = modifyBoard currBoard [('-', pos), ('w', pos - 1)] : generateNewWhiteMovesLeft currBoard (pos + 1)
    | otherwise = generateNewWhiteMovesLeft currBoard (pos + 1)

{-
Arg1: A board
Arg2: A position
OUT: A list of boards that can be generated from Arg1 by moving a white piece left
-}
generateNewWhiteMoveRight :: Board -> Int -> [Board]
generateNewWhiteMoveRight currBoard pos
   | pos > (length currBoard) - 1 = []
   | mod (pos + 1) (boardSize currBoard) == 0 = generateNewWhiteMoveRight currBoard (pos + 1)
   | currBoard !! pos == 'w' && currBoard !! (pos + 1) == '-' = modifyBoard currBoard [('-', pos), ('w', pos + 1)] : generateNewWhiteMoveRight currBoard (pos + 1)
   | otherwise = generateNewWhiteMoveRight currBoard (pos + 1)

{-
Arg1: A board
Arg2: A position
OUT: A list of boards that can be generated from Arg1 by moving the white Flag down
-}
generateNewWhiteFlagMovesUp :: Board -> Int -> [Board]
generateNewWhiteFlagMovesUp currBoard pos
   | pos < (boardSize currBoard) = generateNewWhiteFlagMovesUp currBoard (pos + 1)
   | pos > (length currBoard) - 1 = []
   | currBoard !! pos == 'W' && currBoard !! (pos - (boardSize currBoard)) == '-' = modifyBoard currBoard [('-', pos), ('W', pos - (boardSize currBoard))] : generateNewWhiteFlagMovesUp currBoard (pos + 1)
   | otherwise = generateNewWhiteFlagMovesUp currBoard (pos + 1)


{-
Arg1: A board
Arg2: A position
OUT: A list of boards that can be generated from Arg1 by moving the white Flag up
-}
generateNewWhiteFlagMovesDown :: Board -> Int -> [Board]
generateNewWhiteFlagMovesDown currBoard pos
   | pos > ((length currBoard) - (boardSize currBoard) - 1) = []
   | currBoard !! pos == 'W' && currBoard !! (pos + (boardSize currBoard)) == '-' = modifyBoard currBoard [('-', pos), ('W', pos + (boardSize currBoard))] : generateNewWhiteFlagMovesDown currBoard (pos + 1)
   | otherwise = generateNewWhiteFlagMovesDown currBoard (pos + 1)

{-
Arg1: A board
Arg2: A position
OUT: A list of boards that can be generated from Arg1 by moving the white Flag left
-}
generateNewWhiteFlagMovesLeft :: Board -> Int -> [Board]
generateNewWhiteFlagMovesLeft currBoard pos
   | pos > (length currBoard) - 1 = []
   | mod (pos) (boardSize currBoard) == 0 = generateNewWhiteFlagMovesLeft currBoard (pos + 1)
   | currBoard !! pos == 'W' && currBoard !! (pos - 1) == '-' = modifyBoard currBoard [('-', pos), ('W', pos - 1)] : generateNewWhiteFlagMovesLeft currBoard (pos + 1)
   | otherwise = generateNewWhiteFlagMovesLeft currBoard (pos + 1)


{-
Arg1: A board
Arg2: A position
OUT: A list of boards that can be generated from Arg1 by moving the white Flag right
-}
generateNewWhiteFlagMovesRight :: Board -> Int -> [Board]
generateNewWhiteFlagMovesRight currBoard pos
   | pos > (length currBoard) - 1 = []
   | mod (pos + 1) (boardSize currBoard) == 0 = generateNewWhiteFlagMovesRight currBoard (pos + 1)
   | currBoard !! pos == 'W' && currBoard !! (pos + 1) == '-' = modifyBoard currBoard [('-', pos), ('W', pos + 1)] : generateNewWhiteFlagMovesRight currBoard (pos + 1)
   | otherwise = generateNewWhiteFlagMovesRight currBoard (pos + 1)

{-
Arg1: A board
Arg2: A position
OUT: A list of boards that can be generated from Arg1 by jumping a white piece to the left
-}
generateNewWhiteJumpsLeft :: Board -> Int -> [Board]
generateNewWhiteJumpsLeft currBoard pos
   | pos > (length currBoard) - 1 = []
   | mod pos (boardSize currBoard) == 0 || mod (pos - 1) (boardSize currBoard) == 0 = generateNewWhiteJumpsLeft currBoard (pos + 1)
   | currBoard !! pos == 'w' && (currBoard !! (pos - 1) == 'b' || currBoard !! (pos - 1) == 'B') && currBoard !! (pos - 2) == '-' = modifyBoard currBoard [('-', pos), ('-', pos - 1), ('w', pos - 2)] : generateNewWhiteJumpsLeft currBoard (pos + 1)
   | otherwise = generateNewWhiteJumpsLeft currBoard (pos + 1)

{-
Arg1: A board
Arg2: A position
OUT: A list of boards that can be generated from Arg1 by jumping a white piece to the right
-}
generateNewWhiteJumpsRight :: Board -> Int -> [Board]
generateNewWhiteJumpsRight currBoard pos
   | pos > (length currBoard) - 1 = []
   |mod (pos + 1) (boardSize currBoard) == 0 || mod (pos + 2) (boardSize currBoard) == 0 = generateNewWhiteJumpsDown currBoard (pos + 1)
   | currBoard !! pos == 'w' && (currBoard !! (pos + 1)  == 'b' || currBoard !! (pos + 1) == 'B') && currBoard !! (pos + 2) == '-' = modifyBoard currBoard[('-', pos), ('-', pos + 1), ('w', pos + 2)] : generateNewWhiteJumpsRight currBoard (pos + 1)
   | otherwise = generateNewWhiteJumpsRight currBoard (pos + 1)


{-
Arg1: A board
Arg2: A position
OUT: A list of boards that can be generated from Arg1 by jumping a white piece down
-}
generateNewWhiteJumpsDown :: Board -> Int -> [Board]
generateNewWhiteJumpsDown currBoard pos
   | pos > (length currBoard) - (2 * boardSize currBoard) - 1 = []
   | currBoard !! pos == 'w' && (currBoard !! (pos + (boardSize currBoard)) == 'b' || currBoard !! (pos + (boardSize currBoard)) == 'B')
   && currBoard !! (pos + (2 * boardSize currBoard)) == '-' = modifyBoard currBoard [('-', pos), ('-', pos + boardSize currBoard),
   ('w', pos + (2 * boardSize currBoard))] : generateNewWhiteJumpsDown currBoard (pos + 1)
   | otherwise = generateNewWhiteJumpsDown currBoard (pos + 1)


{-
Arg1: the current board configuration
OUT: A list of all the possible moves the 'w' player can make
-}
generateNewBoards :: Board -> [Board]
generateNewBoards currBoard = 
   concat [generateNewWhiteMovesDown currBoard 0, generateNewWhiteMovesLeft currBoard 0,
   generateNewWhiteMoveRight currBoard 0, generateNewWhiteFlagMovesDown currBoard 0,
   generateNewWhiteFlagMovesUp currBoard 0, generateNewWhiteFlagMovesLeft currBoard 0,
   generateNewWhiteFlagMovesRight currBoard 0, generateNewWhiteJumpsLeft currBoard 0,
   generateNewWhiteJumpsRight currBoard 0, generateNewWhiteJumpsDown currBoard 0]

{-
Arg1: A board
OUT: Size of the board (the number of rows/cols)
-}
boardSize :: Board -> Int
boardSize board = round (sqrt (fromIntegral (length board)))

{-
Arg1: A list of boards
OUT: A list of boards flipped 'w' and 'b'
-}
flipEachBoard :: [Board] -> [Board]
flipEachBoard boards = map flipBoard boards

{-
Arg1: A board
OUT: Arg1 with 'w' swapped with 'b' and 'W' swapped with 'B'
-}
flipBoard :: Board -> Board
flipBoard [] = []
flipBoard ('-':as) = '-' : (flipBoard as)
flipBoard('w':as) = 'b' : (flipBoard as)
flipBoard('b':as) = 'w' : (flipBoard as)

{-
Arg1: A string
Arg2: An index
Arg3: A value
OUT: Arg1 with the character at position Arg2 replaced by Arg3
-}
replaceNth :: String -> Int -> Char -> String
replaceNth (x:xs) 0 newVal = newVal:xs
replaceNth (x:xs) pos newVal = x:replaceNth xs (pos - 1) newVal


{-
Arg1: A board
Arg2: A list of tuples (val, pos)
OUT: Arg1 with characters at position pos replaced by val.
-}
modifyBoard :: Board -> [(Char, Int)] -> Board
modifyBoard board [] = board
modifyBoard board ((val, pos) : tail) = modifyBoard (replaceNth board pos val) tail
-- modifyBoard board ((val, pos) : t) = 






-----------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------MOVE GENERATION-------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------

