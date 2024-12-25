import Control.Applicative
import Data.Char (digitToInt)
import Data.IntMap qualified as IM
import Data.List
import Data.Map qualified as M
import System.Environment

type Coord = (Int, Int)

numpad :: M.Map (Code Int) Coord
numpad =
    M.fromList
        [ (Val 7, (0, 3))
        , (Val 8, (1, 3))
        , (Val 9, (2, 3))
        , (Val 4, (0, 2))
        , (Val 5, (1, 2))
        , (Val 6, (2, 2))
        , (Val 1, (0, 1))
        , (Val 2, (1, 1))
        , (Val 3, (2, 1))
        , (Val 0, (1, 0))
        , (Press, (2, 0))
        ]

dirpad :: M.Map (Code Dir) Coord
dirpad =
    M.fromList
        [ (Val U, (1, 1))
        , (Press, (2, 1))
        , (Val L, (0, 0))
        , (Val D, (1, 0))
        , (Val R, (2, 0))
        ]
dirPaths :: M.Map (Code Dir, Code Dir) [Dir]
dirPaths =
    M.fromList
        [ ((Press, Press), [])
        , ((Val U, Val U), [])
        , ((Val D, Val D), [])
        , ((Val L, Val L), [])
        , ((Val R, Val R), [])
        , ((Press, Val U), [L])
        , ((Press, Val D), [L, D])
        , ((Press, Val R), [D])
        , ((Press, Val L), [D, L, L])
        , ((Val U, Press), [R])
        , ((Val U, Val D), [D])
        , ((Val U, Val R), [D, R])
        , ((Val U, Val L), [D, L])
        , ((Val D, Press), [U, R])
        , ((Val D, Val U), [U])
        , ((Val D, Val R), [R])
        , ((Val D, Val L), [L])
        , ((Val R, Press), [U])
        , ((Val R, Val U), [L, U])
        , ((Val R, Val D), [L])
        , ((Val R, Val L), [L, L])
        , ((Val L, Press), [R, R, U])
        , ((Val L, Val U), [R, U])
        , ((Val L, Val R), [R, R])
        , ((Val L, Val D), [R])
        ]

getHorizontalTravel k = if k < 0 then replicate (negate k) R else replicate k L
getVerticalTravel k = if k < 0 then replicate (negate k) U else replicate k D

data Dir = U | D | L | R deriving (Show, Eq)
instance Ord Dir where
    compare a b = compare (weight a) (weight b)
      where
        weight U = 2
        weight R = 3
        weight D = 1
        weight L = 0

data Code a = Val a | Press deriving (Eq, Ord)
instance (Show a) => Show (Code a) where
    show (Val i) = show i
    show Press = "A"
prettyPrint :: Code Dir -> String
prettyPrint (Val U) = "^"
prettyPrint (Val L) = "<"
prettyPrint (Val R) = ">"
prettyPrint (Val D) = "v"
prettyPrint Press = "A"

prettyPrintL :: [Code Dir] -> String
prettyPrintL s = show $ concatMap (filter (/= ',') . prettyPrint) s

newtype Cursor a = Cursor (Code a) deriving (Show)

class Mover a where
    initial :: Cursor a
    moveTo :: Cursor a -> Code a -> ([Dir], Cursor a)

instance Mover Int where
    initial = Cursor Press
    moveTo (Cursor curr) target = (intPath curr target, Cursor target)
      where
        intPath i j
            | i == Press || i == Val 0 = (getVerticalTravel (iRow - jRow) ++ getHorizontalTravel (iCol - jCol))
            | j == Press || j == Val 0 = (getHorizontalTravel (iCol - jCol) ++ getVerticalTravel (iRow - jRow))
            | otherwise = (getHorizontalTravel (iCol - jCol) ++ getVerticalTravel (iRow - jRow))
          where
            (iCol, iRow) = numpad M.! i
            (jCol, jRow) = numpad M.! j
instance Mover Dir where
    initial = Cursor Press
    moveTo (Cursor curr) target = (dirPath curr target, Cursor target)
      where
        dirPath i j
            | i == Val L = getHorizontalTravel (iCol - jCol) ++ getVerticalTravel (iRow - jRow)
            | j == Val L = (getVerticalTravel (iRow - jRow) ++ getHorizontalTravel (iCol - jCol))
            | otherwise = (getHorizontalTravel (iCol - jCol) ++ getVerticalTravel (iRow - jRow))
          where
            (iCol, iRow) = dirpad M.! i
            (jCol, jRow) = dirpad M.! j

parseCodes :: String -> [Code Int]
parseCodes = map (\c -> if c == 'A' then Press else Val (digitToInt c))

getBtnSequence :: (Mover a) => [Code a] -> [Code Dir]
getBtnSequence cs = fst $ foldl (\(d, r) c -> let (d', r') = moveTo r c in (d ++ map Val d' ++ [Press], r')) ([], initial) cs

getNRobotDirSequence :: (Mover a) => [Code a] -> Int -> [Code Dir]
getNRobotDirSequence seq 0 = getBtnSequence seq
getNRobotDirSequence seq n = fst $ foldl (\(ds, c) k -> (ds ++ map Val (dirPaths M.! (c, k)) ++ [Press], k)) ([], Press) nxt
  where
    -- getNRobotDirSequence seq n = fst $ foldl (\(d, r) c -> let (d', r') = moveTo r c in (d ++ map Val (d') ++ [Press], r')) ([], initial) nxt

    nxt = getNRobotDirSequence seq (n - 1)

main :: IO ()
main = do
    [fileLoc] <- getArgs
    codes@(init : _) <- map parseCodes . lines <$> readFile fileLoc
    -- Port 1
    let combined = zip (map (`getNRobotDirSequence` 2) codes) codes
    -- print $ map (\(s, c) -> (length s, foldl (\acc (Val x) -> acc * 10 + x) 0 $ filter (/= Press) c)) combined
    -- print $ map (uncurry (*)) $ map (\(s, c) -> (length s, foldl (\acc (Val x) -> acc * 10 + x) 0 $ filter (/= Press) c)) combined
    print $ sum $ map (uncurry (*)) $ map (\(s, c) -> (length s, foldl (\acc (Val x) -> acc * 10 + x) 0 $ filter (/= Press) c)) combined

    print codes
    putStrLn $ concatMap (prettyPrintL) $ map (`getNRobotDirSequence` 0) codes
    putStrLn $ concatMap (prettyPrintL) $ map (`getNRobotDirSequence` 1) codes
    putStrLn $ concatMap (prettyPrintL) $ map (`getNRobotDirSequence` 2) codes
    let (d, upd) = moveTo (initial :: Cursor (Int)) (Val 7)
    print d
    print upd
    print $ moveTo upd (Val 3)

-- print $ getBtnSequence init
-- print $ getNRobotDirSequence init 0
-- print $ getNRobotDirSequence init 1
-- putStrLn $ filter (/= ',') $ show $ getNRobotDirSequence init 2

-- Part 1
