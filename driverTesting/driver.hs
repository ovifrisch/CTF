import qualified Ctf1
import qualified Ctf2

driver :: [String] -> Int -> Bool -> Int -> [String]
driver boards depth turn turns
    | Ctf2.checkBlackWin (head boards) == True || Ctf2.checkWhiteWin(head boards) == True = boards
    | turns == 0        = boards
    | turn == True      = let x = (Ctf1.capture boards 'w' depth) in (driver (x:boards) depth False (turns - 1))
    | turn == False     = let x = (Ctf2.capture boards 'b' depth) in (driver (x:boards) depth True (turns - 1))
