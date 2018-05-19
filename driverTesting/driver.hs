import qualified Ctf1
import qualified Ctf2

driver1 :: [String] -> Int -> Bool -> Int -> [String]
driver1 boards depth turn turns
    | Ctf2.checkBlackWin (head boards) == True = ("2 wins":boards)
    | Ctf2.checkWhiteWin(head boards) == True = ("1 wins":boards)
    | turns == 0        = boards
    | turn == True      = let x = (Ctf1.capture boards 'w' depth) in (driver1 (x:boards) depth False (turns - 1))
    | turn == False     = let x = (Ctf2.capture boards 'b' depth) in (driver1 (x:boards) depth True (turns - 1))

driver2 :: [String] -> Int -> Bool -> Int -> [String]
driver2 boards depth turn turns
    | Ctf2.checkBlackWin (head boards) == True = ("1 wins":boards)
    | Ctf2.checkWhiteWin(head boards) == True = ("2 wins":boards)
    | turns == 0        = boards
    | turn == True      = let x = (Ctf2.capture boards 'w' depth) in (driver2 (x:boards) depth False (turns - 1))
    | turn == False     = let x = (Ctf1.capture boards 'b' depth) in (driver2 (x:boards) depth True (turns - 1))



trial1 :: [String]
trial1 = (driver1 ["-wWw--www-------bbb--bBb-"] 3 True 150)

trial2 :: [String]
trial2 = (driver1 ["--Ww--w-w-------b-b--bB--"] 3 True 150)

trial3 :: [String]
trial3 = (driver1 ["-wWw--www-------bbb--bBb-"] 4 True 150)

trial4 :: [String]
trial4 = (driver1 ["Ww------------bB"] 2 True 150)

trial5 :: [String]
trial5 = (driver1 ["Ww--w------b--bB"] 5 True 150)

trial6 :: [String]
trial6 = (driver2 ["-wWw--www-------bbb--bBb-"] 3 True 150)

trial7 :: [String]
trial7 = (driver2 ["--Ww--w-w-------b-b--bB--"] 3 True 150)

trial8 :: [String]
trial8 = (driver2 ["-wWw--www-------bbb--bBb-"] 4 True 150)

trial9 :: [String]
trial9 = (driver2 ["Ww------------bB"] 2 True 150)

trial10 :: [String]
trial10 = (driver2 ["Ww--w------b--bB"] 5 True 150)


alltrials :: ([String],(Int,Int))
alltrials = let x = [(head trial1), (head trial2), (head trial3), (head trial4), (head trial5), (head trial6), (head trial7), (head trial8), (head trial9), (head trial10)]
    in (x, countWins x)

countWins :: [String] -> (Int,Int)
countWins ("1 wins":[]) = (1, 0)
countWins ("2 wins":[]) = (0, 1)
countWins ("1 wins":xs) = (fst (countWins xs) + 1, snd (countWins xs))
countWins ("2 wins":xs) = (fst (countWins xs), snd (countWins xs) + 1)
