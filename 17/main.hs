import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Data.Bits
import Data.Char
import Data.List
import Data.Map qualified as M
import Data.STRef
import Data.Set qualified as S
import System.Environment
import System.Posix (BaudRate (B0))

-- type ProgState = M.Map Char Int

data ProgState a = PS
    { a :: a
    , b :: a
    , c :: a
    }
    deriving (Show)

getVal :: ProgState Int -> Int -> Int
getVal (PS a _ _) 4 = a
getVal (PS _ b _) 5 = b
getVal (PS _ _ c) 6 = c
getVal _ n
    | n < 4 = n
    | otherwise = error "got invalid combo val"

type Prog = (ProgState Int, [Int])

parseProg' :: [String] -> Prog
parseProg' s = (s', p')
  where
    ([a, b, c], [p]) = (map (drop 11) $ takeWhile (not . null) s, drop 1 $ dropWhile (not . null) s)
    s' = PS (read a) (read b) (read c)
    p' = map digitToInt $ fst $ partition (/= ',') $ drop 9 p

data Instr a = ADV a | BXL a | BST a | JNZ Int | BXC a | OUT a | BDV a | CDV a deriving (Show)

parseProg :: [String] -> (ProgState Int, [Instr Int])
parseProg s = (s', mapping p')
  where
    ([a, b, c], [p]) = (map (drop 11) $ takeWhile (not . null) s, drop 1 $ dropWhile (not . null) s)
    s' = PS (read a) (read b) (read c)
    p' = map digitToInt $ fst $ partition (/= ',') $ drop 9 p
    mapping [] = []
    mapping (0 : c_ : is) = ADV c_ : mapping is
    mapping (1 : c_ : is) = BXL c_ : mapping is
    mapping (2 : c_ : is) = BST c_ : mapping is
    mapping (3 : c_ : is) = JNZ (c_ `div` 2) : mapping is
    mapping (4 : c_ : is) = BXC c_ : mapping is
    mapping (5 : c_ : is) = OUT c_ : mapping is
    mapping (6 : c_ : is) = BDV c_ : mapping is
    mapping (7 : c_ : is) = CDV c_ : mapping is

eval2 :: Prog -> String
eval2 = intersperse ',' . eval' 0
  where
    eval' :: Int -> Prog -> String
    eval' pc (st, instrs)
        | pc >= length instrs = ""
        | otherwise = r ++ eval' pc' (st', instrs)
      where
        (pc', st', r) = case take 2 $ drop pc instrs of
            [0, c_] -> (pc + 2, st{a = truncate (fromIntegral (a st) / (2 ^ getVal st c_))}, "")
            [1, c_] -> (pc + 2, st{b = b st `xor` c_}, "")
            [2, c_] -> (pc + 2, st{b = getVal st c_ `mod` 8}, "")
            [3, c_] -> (if a st /= 0 then c_ else pc + 2, st, "")
            [4, _] -> (pc + 2, st{b = b st `xor` c st}, "")
            [5, c_] -> (pc + 2, st, show $ getVal st c_ `mod` 8)
            [6, c_] -> (pc + 2, st{b = truncate (fromIntegral (a st) / (2 ^ getVal st c_))}, "")
            [7, c_] -> (pc + 2, st{c = truncate (fromIntegral (a st) / (2 ^ getVal st c_))}, "")
            _ -> error "What"

eval :: (ProgState Int, [Instr Int]) -> String
eval = intersperse ',' . eval' 0
  where
    eval' :: Int -> (ProgState Int, [Instr Int]) -> String
    eval' pc (st, instrs)
        | pc >= length instrs = ""
        | otherwise = r ++ eval' pc' (st', instrs)
      where
        (pc', st', r) = case instrs !! pc of
            (ADV c_) -> (pc + 1, st{a = truncate (fromIntegral (a st) / (2 ^ getVal st c_))}, "")
            (BXL c_) -> (pc + 1, st{b = b st `xor` c_}, "")
            (BST c_) -> (pc + 1, st{b = getVal st c_ `mod` 8}, "")
            (JNZ c_) -> (if a st /= 0 then c_ else pc + 1, st, "")
            (BXC _) -> (pc + 1, st{b = b st `xor` c st}, "")
            (OUT c_) -> (pc + 1, st, show $ getVal st c_ `mod` 8)
            (BDV c_) -> (pc + 1, st{b = truncate (fromIntegral (a st) / (2 ^ getVal st c_))}, "")
            (CDV c_) -> (pc + 1, st{c = truncate (fromIntegral (a st) / (2 ^ getVal st c_))}, "")

filterMap :: (a -> Maybe b) -> [a] -> [b]
filterMap f = foldr g mempty
  where
    g v acc = case f v of
        Just x' -> x' : acc
        Nothing -> acc
test :: [Instr Int] -> [Int] -> [Int]
test _ [] = []
test i xs = xs' : test i (drop i' xs)
  where
    (Just (xs', i')) = find (\(v, d) -> listify (eval (PS v 0 0, i)) == take d xs) l
    l = [(val v, digits v) | v <- [0 ..]]
    val v
        | v > 7 = (v `shiftR` 3) `shiftL` val (v `mod` 8)
        | otherwise = v
    digits v = v `div` 8
    listify s = read $ "[" ++ s ++ "]"

test2 :: [Instr Int] -> [Int] -> [Maybe Int]
test2 _ [] = []
test2 i (x : xs) = xs' : test2 i xs
  where
    (xs') = find (\v -> digitToInt (head (eval (PS v 0 0, i))) == x) [0 .. 7]

-- Binary search (hehe)
force :: [Instr Int] -> [Int] -> Int -> Int -> Int
force ins res min' max' = sum val
  where
    val = listify $ eval (PS max' 0 0, ins)
    listify :: String -> [Int]
    listify s = read $ "[" ++ s ++ "]"

whatever :: Int -> Int
whatever val = if big > 0 then (imm * 10) + whatever big else imm
  where
    (big, imm) = val `divMod` 8

main :: IO ()
main = do
    [fileloc] <- getArgs
    prog@(initState, instr) <- parseProg . lines <$> readFile fileloc
    (_, instrRaw) <- parseProg' . lines <$> readFile fileloc
    let executed = eval prog
    -- Part 1
    putStrLn $ "out: " ++ executed
    -- print $ find (\i -> "2,4,1" `isPrefixOf` eval (PS i 0 0, instr)) [0 ..]
    -- print $ eval (PS 485 0 0, instr)
    -- print $ eval (PS 6653 0 0, instr)
    -- print $ eval (PS (6653 + 485) 0 0, instr)
    -- let shouldBe = init $ tail $ show instrRaw
    -- putStrLn shouldBe
    -- print $ find (\i -> eval (PS i 0 0, instr) == shouldBe) [a initState ..]
    print $ eval (PS 108107566389757 0 0, instr)
    print $ eval (PS 9641146161661 0 0, instr)

-- prog:
-- b = a mod 8
-- b = b mod 3
-- c = a / (2 ^ b)
-- b = b xor c
-- b = b mod 3
-- a = a / (2^3)
-- out (b mod 8)
-- JMP 0
--
-- OUT a = (((b' xor c ) mod 3) mod 8) ++ OUT (a / (2^b'))
--      where b' = (a mod 8) mod 3
--
--  b1 = (a mod 8) mod 3
--  c = a / 2^(b1)
--  b2 = (b1 xor c) mod 3
--  a' = a / 8
--  OUT b2
--  recurive w/a' if a' /= 0
