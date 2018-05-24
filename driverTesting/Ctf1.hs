module Ctf1 where

type Board = String

{-
Arg1: A list of boards representing the history of the game
Arg2: A char indicating whether it's 'w' or 'b' turn to move
Arg3: An int indicating how many moves ahead the minimax algorithm is allowed to search
OUT: A board representing the next best move
-}
capture :: [Board] -> Char -> Int -> Board
capture (current:history) player movesAhead
   | checkWhiteWin current || checkWhiteWin (flipBoard current) = "Game over"
   | player == 'w'                                          = fst (toplevelMinimax current movesAhead True history)
   | player == 'b'                                          = flipBoard (fst(toplevelMinimax (flipBoard current) movesAhead True (flipEachBoard history)))


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
Args are same as minimax, just with single board instead of lists of boards
-}
toplevelMinimax :: Board -> Int -> Bool -> [Board] -> (Board, Float)
toplevelMinimax board height minOrmax history
   | minOrmax == True && (filterBoards (generateNewBoards board) history) == [] = (board, goodness board)
   | minOrmax == False && (filterBoards (flipEachBoard (generateNewBoards (flipBoard board))) history) == [] = (board, goodness board)
   | height == 0 = (board, goodness board)
   | minOrmax == True = minimax (filterBoards (generateNewBoards board) history) (height - 1) True history
   | otherwise = minimax (filterBoards (flipEachBoard (generateNewBoards (flipBoard board))) history) (height - 1) False history

{-
Arg1: A list of boards
Arg2: height of the node
Arg3: True for max, False for min
Arg4: The previous boards, not allowed to be played again
OUT: The next best move, and its heuristic strength
-}
minimax :: [Board] -> Int -> Bool -> [Board] -> (Board, Float)
minimax (board : []) height minOrmax history
   | checkWhiteWin (flipBoard board) == True || checkWhiteWin board == True = (board,goodness board)
   | minOrmax == True && (filterBoards (generateNewBoards board) history) == [] = (board, goodness board)
   | minOrmax == False && (filterBoards (flipEachBoard (generateNewBoards (flipBoard board))) history) == [] = (board, goodness board)
   | height == 0 = (board, goodness board)
   | minOrmax == True = (board, snd (minimax (filterBoards (generateNewBoards board) history) (height - 1) True history))
   | otherwise = (board, snd (minimax (filterBoards (flipEachBoard (generateNewBoards (flipBoard board))) history) (height - 1) False history))

minimax (board : boards) height minOrmax history
   | null (tail boards) && minOrmax == True = maxBoard [(minimax [board] (height) False history), (minimax boards (height) False history)]
   | null (tail boards) = minBoard [(minimax [board] (height) True history), (minimax boards (height) True history)]
   | minOrmax == True = maxBoard [(minimax [board] (height) False history), (minimax boards (height) True history)]
   | otherwise = minBoard [(minimax [board] (height) True history), (minimax boards (height) False history)]


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
goodness board
   | checkWhiteWin board = 10000
   | checkWhiteWin (flipBoard board) = -10000
   | otherwise           = (flagDistance board) * (-100) + (pawnDiff board) * (5) + flagDistance (flipBoard board) * (20) + (nearbyFriends board 0) * (5)
      + (pawnPast board) * (-100) + (pawnPast (flipBoard board)) * (15) + (pawnDistanceForward board) * (-5)
      + (flagSurround board 0) * (-25)

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
   | pos > ((length currBoard) - (boardSize currBoard) - 1) = []
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
generateNewWhiteMovesRight :: Board -> Int -> [Board]
generateNewWhiteMovesRight currBoard pos
   | pos > (length currBoard) - 1 = []
   | mod (pos + 1) (boardSize currBoard) == 0 = generateNewWhiteMovesRight currBoard (pos + 1)
   | currBoard !! pos == 'w' && currBoard !! (pos + 1) == '-' = modifyBoard currBoard [('-', pos), ('w', pos + 1)] : generateNewWhiteMovesRight currBoard (pos + 1)
   | otherwise = generateNewWhiteMovesRight currBoard (pos + 1)

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
   | mod (pos + 1) (boardSize currBoard) == 0 || mod (pos + 2) (boardSize currBoard) == 0 = generateNewWhiteJumpsRight currBoard (pos + 1)
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
   generateNewWhiteMovesRight currBoard 0, generateNewWhiteFlagMovesDown currBoard 0,
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
flipBoard board = reverse (flipBoardHelper board)

flipBoardHelper [] = []
flipBoardHelper ('-':as) = '-' : (flipBoardHelper as)
flipBoardHelper('w':as) = 'b' : (flipBoardHelper as)
flipBoardHelper('b':as) = 'w' : (flipBoardHelper as)
flipBoardHelper('W':as) = 'B' : (flipBoardHelper as)
flipBoardHelper('B':as) = 'W' : (flipBoardHelper as)


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


-----------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------UTILITY FUCTIONS FOR EVALUATION----------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------



---------------------------------------
------ Win Condition Checkers ---------
---------------------------------------

{-
Win condition checkers
Note that checkWhiteForward will return false if no black pawn exists, so we could remove checkBlackPawn
Also doesn't check if either player is out of valid moves
-}

checkWhiteWin :: Board -> Bool
checkWhiteWin board = not(checkWhitePawn (flipBoard board) && checkWhiteFlag (flipBoard board)) || checkWhiteForward board || checkBlackCantMove board

checkBlackCantMove :: Board -> Bool
checkBlackCantMove board = checkBlackCantMoveHelper board 0

{-
Arg1: A board
Arg2: current position at the board
OUT: true if you reach end of board, false if a black piece can move
-}
checkBlackCantMoveHelper :: Board -> Int -> Bool
checkBlackCantMoveHelper board pos
   | pos >= length board = True
   | board !! pos == 'B' && blackFlagCanMove board pos = False
   | board !! pos == 'b' && blackPawnCanMove board pos = False
   | otherwise = checkBlackCantMoveHelper board (pos + 1)

{-
Arg1: a board
Arg2: position of current square
Assumption: piece at current square is 'B'
OUT: true if 'B' can move, false otherwise
-}
blackFlagCanMove :: Board -> Int -> Bool
blackFlagCanMove board pos
   | pos >= boardSize board && board !! (pos - boardSize board) == '-' = True
   | pos < (length board - boardSize board) && board !! (pos + boardSize board) == '-' = True
   | not(mod (pos + 1) (boardSize board) == 0) && board !! (pos + 1) == '-' = True
   | not(mod pos (boardSize board) == 0) && board !! (pos - 1) == '-' = True 
   | otherwise = False

{-
Arg1: a board
Arg2: position of current square
Assumption: piece at current square is 'b'
OUT: true is 'b' can move, false otherwise
-}
blackPawnCanMove :: Board -> Int -> Bool
blackPawnCanMove board pos
   | pos >= boardSize board && board !! (pos - boardSize board) == '-' = True
   | pos >= (2 * boardSize board) && (board !! (pos - boardSize board) == 'w' || board !! (pos - boardSize board) == 'W') && board !! (pos - 2 * boardSize board) == '-' = True
   | not(mod (pos + 1) (boardSize board) == 0) && board !! (pos + 1) == '-' = True
   | mod pos (boardSize board) >= 2 && (board !! (pos - 1) == 'w' || board !! (pos - 1) == 'W') && board !! (pos - 2) == '-' = True
   | not(mod pos (boardSize board) == 0) && board !! (pos - 1) == '-' = True
   | mod pos (boardSize board) < (boardSize board - 2) && (board !! (pos + 1) == 'w' || board !! (pos + 1) == 'W') && board !! (pos + 2) == '-' = True
   | otherwise = False

{-
Arg1: board
Check if the white flag is still on the board
-}
checkWhiteFlag :: Board -> Bool
checkWhiteFlag board = elem 'W' board

{-
Arg1: board
Check if a white pawn is still on the board
-}
checkWhitePawn :: Board -> Bool
checkWhitePawn board = elem 'w' board


{-
Arg1: board
Return true if white flag is past all black pawns
-}
checkWhiteForward :: Board -> Bool
checkWhiteForward board = 
    let boardlen = boardSize board
    in whiteForwardHelper board boardlen

{-
Arg1: board
Arg2: side length
Assist in finding if white flag is past black pawns
-}
whiteForwardHelper :: Board -> Int -> Bool
whiteForwardHelper board len 
   | null board                                    = True
   | elem 'b' (drop (length board - len) board)    = False
   | elem 'W' (drop (length board - len) board)    = True
   | otherwise                                     = whiteForwardHelper (take (length board - len) board) len


--------------------------------------
-----------Pawn differnce--------------
---------------------------------------

{-
Arg1: Board
Return: # more pawns that white has compared to black
-}
pawnDiff :: Board -> Float
pawnDiff (x:xs)
   | null xs && x == 'w'        = 1
   | null xs && x == 'b'        = -1
   | null xs                    = 0
   | x == 'w'                   = 1 + pawnDiff(xs)
   | x == 'b'                   = -1 + pawnDiff(xs)
   | otherwise                  = pawnDiff(xs)


-------------------------------------------
---------Flag to side ---------------------
-------------------------------------------
{-
FlagSide
Arg1: board
Return: True if white flag is touching either the right or left edge of the board 
-}

flagSide :: Board -> Float
flagSide board = flagSideHelper board (boardSize board)

{-
FlagSideHelper
Arg1: board
Arg2: len
Return: True if white flag is touching either the right or left edge of the board 
-}

flagSideHelper :: Board -> Int -> Float
flagSideHelper board len
   | elem 'W' (drop (length board - len) board)    = min (fst(rightleftSide (drop (length board - len) board))) (snd(rightleftSide (drop (length board - len) board)))
   | otherwise                                     = flagSideHelper(take (length board - len) board) len


rightleftSide :: Board -> (Float, Float)
rightleftSide ('W':xs) = (0, fromIntegral (length xs))
rightleftSide (_:xs) = (1 + fst (rightleftSide xs), snd(rightleftSide xs))

------------------------------------------------------
--------------- Flag Distance to End -----------------
------------------------------------------------------

{-
 Arg1: board
 Return: Number of rows to pass last enemy pawn.
-}

flagDistance :: Board -> Float
flagDistance board = flagDistanceHelper1 board (boardSize board) / fromIntegral(boardSize board)

flagDistanceHelper1 :: Board -> Int -> Float
flagDistanceHelper1 board len
   | elem 'b' (drop (length board - len) board) = flagDistanceHelper2 board len
   | otherwise                                  = flagDistanceHelper1 (take (length board - len) board) len

flagDistanceHelper2 :: Board -> Int -> Float
flagDistanceHelper2 board len
   | elem 'W' (drop (length board - len) board) = 0
   | otherwise                                  = 1 + flagDistanceHelper2 (take (length board - len) board) len

----------------------------------------------
---pawns past opponents flag -----------------
----------------------------------------------

{-
  Takes a board, returns the number of white pawns past the black flag
-}
pawnPast :: Board -> Float
pawnPast board
    | elem 'B' (drop (length board - (boardSize board)) board) = 0
    | otherwise = (countPiece (drop (length board - (boardSize board)) board) 'w') + pawnPast (take (length board - (boardSize board)) board)

{-
 Takes a string and a character (piece indicator)
 counts the number of times that char appears in that string
-}
countPiece :: String -> Char -> Float
countPiece board c 
    | null board = 0
    | (head board) == c = 1 + countPiece (tail board) c
    | otherwise = countPiece (tail board) c


-----------------------------------------------
--------------Nearby Friends ------------------
-----------------------------------------------

{-
   Returns the number of white pieces touching other white pieces
-}

nearbyFriends :: Board ->  Int -> Float
nearbyFriends board pos
   | pos > (length board) - 1 = 0
   | board !! pos == 'w' || board !! pos == 'W' = (countPiece (getSurrounding board pos) 'w') + (countPiece (getSurrounding board pos) 'W') + nearbyFriends board (1+pos) 
   | otherwise = nearbyFriends board (pos+1)

{-
Takes a board and position
Returns all the pieces around said position
-}
getSurrounding :: Board -> Int -> String
getSurrounding board pos = [(getRight board pos),(getLeft board pos),(getAbove board pos),(getBelow board pos)]

{-
Each of the below functions take a position and a board and return the character either left, right, above or below that postion
A 'N' is returned if the position is not valid
-}

getRight :: Board -> Int -> Char
getRight board pos
   | mod (pos + 1) (boardSize board) == 0 = 'N'
   | otherwise = board !! (pos + 1)

getLeft :: Board -> Int -> Char
getLeft board pos 
   | mod (pos) (boardSize board) == 0 = 'N'
   | otherwise = board !! (pos - 1)

getAbove :: Board -> Int -> Char
getAbove board pos
   | pos < (boardSize board) = 'N'
   | otherwise = board !! (pos - (boardSize board))

getBelow :: Board -> Int -> Char
getBelow board pos
   | pos > ((length board) - (boardSize board) - 1) = 'N'
   | otherwise = board !! (pos + (boardSize board))

----------------------------------
-- pawnDistanceForward -----------
----------------------------------

{-
Takes a board and returns the total forward movement of the white pawns
-}

pawnDistanceForward :: Board -> Float
pawnDistanceForward board = pdfHelper board 0 (boardSize board) / fromIntegral (boardSize board)

pdfHelper :: Board -> Float -> Int -> Float
pdfHelper board row len
   | null board = 0
   | otherwise  = row * (countPiece (take len board) 'w') + pdfHelper (drop len board) (row + 1) len


----------------------------------------
----- Flag surroundings ----------------
----------------------------------------

{-
 Returns number of opposing pieces around the flag
-}

flagSurround :: Board -> Int -> Float
flagSurround board pos 
   | board !! pos == 'W' = (countPiece (getDiagAndSurrounding board pos) 'b') / 6
   | otherwise = flagSurround board (pos + 1)

{-
Takes a board and position
Returns all the pieces around said position, and in the front 2 diagnols of said position
-}
getDiagAndSurrounding :: Board -> Int -> String
getDiagAndSurrounding board pos = [(getRight board pos),(getLeft board pos),(getAbove board pos),(getBelow board pos),(getDiagLeft board pos),(getDiagRight board pos)]

getDiagLeft :: Board -> Int -> Char
getDiagLeft board pos
   | pos < (boardSize board) = 'N'
   | otherwise = getLeft board (pos - (boardSize board))


getDiagRight :: Board -> Int -> Char
getDiagRight board pos
   | pos < (boardSize board) = 'N'
   | otherwise = getRight board (pos - (boardSize board))

{-
Returns 1 if a pawn is in the same col as the opponents flag
-}

pawnCol :: Board -> Int -> Float
pawnCol board pos
   | board !! pos == 'B' = (pawnColHelper board (pos - boardSize board))
   | otherwise = pawnCol board (pos + 1)

pawnColHelper :: Board -> Int -> Float
pawnColHelper board pos
   | board !! pos == 'w' = 1 
   | (pos - boardSize board) < (length board - 1) = 0
   | otherwise = (pawnColHelper board (pos - boardSize board))

