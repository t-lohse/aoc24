import Control.Applicative
import Data.List
import System.Environment

parseTests :: String -> (Int, [Int])
parseTests s = (i, l)
  where
    i = read $ takeWhile (/= ':') s
    l = map read $ words $ drop 1 $ dropWhile (/= ':') s

type BinOp a = a -> a -> a

t :: (Ord a, Num a) => a -> a -> [a] -> [BinOp a] -> Bool
t target val [] _ = target == val
t target val (x : xs) ops
    | val > target = False
    | otherwise = any (\o -> t target (o val x) xs ops) ops

concatI :: BinOp Int
concatI a b = read $ show a ++ show b

main = do
    [fileloc] <- getArgs
    file <- lines <$> readFile fileloc
    let parsed = map parseTests file
    -- Part 1
    print $ sum $ map fst $ filter (\(a, b) -> t a 0 b [(+), (*)]) parsed
    -- Part 2
    print $ sum $ map fst $ filter (\(a, b) -> t a 0 b [(+), (*), concatI]) parsed
