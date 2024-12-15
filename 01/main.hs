import Data.List
import Data.Map qualified as M
import System.Environment

convert :: [String] -> ([Int], [Int])
convert [] = ([], [])
convert (x : xs) = (read a : as, read b : bs)
  where
    (as, bs) = convert xs
    [a, b] = words x

sumCached :: M.Map Int Int -> [Int] -> [Int] -> Int
sumCached _ [] _ = 0
sumCached m (a : as) l = case M.lookup a m of
    Nothing -> (a * c) + sumCached (M.insert a c m) as l
    Just v -> (a * v) + sumCached m as l
  where
    c = length $ filter (== a) l

main :: IO ()
main = do
    [fileLoc] <- getArgs
    file <- readFile fileLoc

    -- Part 1
    let (a, b) = convert $ filter (not . null) $ lines file
        (a', b') = (sort a, sort b)
        l = zipWith ((abs .) . (-)) a' b'
    print (sum l)

    -- Part 2
    print (sumCached M.empty a' b')
