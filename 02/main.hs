import Control.Applicative
import System.Environment

safeCond :: (Num a, Ord a) => Ordering -> a -> a -> Bool
safeCond o a b = compare a b == o && (diff <= 3) && (diff >= 1)
  where
    diff = abs (a - b)

isSafe :: (Num a, Ord a) => [a] -> Bool
isSafe = liftA2 (||) (isSafe' LT) (isSafe' GT)

isSafe' :: (Ord a, Num a) => Ordering -> [a] -> Bool
isSafe' o (x : y : xs) = safeCond o x y && isSafe' o (y : xs)
isSafe' _ _ = True

-- More systematic approach
isSafeAdapting :: (Num a, Ord a) => [a] -> Bool
isSafeAdapting = liftA2 (||) (isSafeAdapting' LT []) (isSafeAdapting' GT [])

isSafeAdapting' :: (Num a, Ord a) => Ordering -> [a] -> [a] -> Bool
isSafeAdapting' o l (x : y : xs)
    | safeCond o x y = isSafeAdapting' o (l ++ [x]) (y : xs)
    | otherwise = isSafe' o (l ++ (x : xs)) || isSafe' o (l ++ (y : xs))
isSafeAdapting' _ _ _ = True

-- Brute force
removeOne :: [a] -> [[a]]
removeOne [] = []
removeOne (x : xs) = xs : map (x :) (removeOne xs)

main :: IO ()
main = do
    [fileLoc] <- getArgs
    file <- readFile fileLoc
    let list = map (map read . words) $ lines file

    -- Part 1
    print $ length $ filter isSafe list

    -- Part 2
    let sys = filter isSafeAdapting list
        brute = filter (any isSafe . removeOne) list
    print $ length sys
    print $ length brute
    putStrLn "Differences:"
    print $ filter (`notElem` sys) brute
    print $ filter (`notElem` brute) sys
