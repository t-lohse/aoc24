import Data.Map qualified as M
import System.Environment

blink :: Int -> [Int]
blink 0 = [1]
blink x
    | even (length x') = [read a, read b]
    | otherwise = [x * 2024]
  where
    x' = show x
    (a, b) = splitAt (length x' `div` 2) x'

type Bag = M.Map Int Int

totalStones :: Bag -> Int
totalStones = sum

step' :: Bag -> Bag
step' = M.foldrWithKey folder M.empty
  where
    folder :: Int -> Int -> Bag -> Bag
    folder stoneValue stoneCount bag = foldr insert bag newValues
      where
        newValues = blink stoneValue
        insert newValue = M.insertWith (+) newValue stoneCount

main :: IO ()
main = do
    [fileloc] <- getArgs
    stones <- map read . words . head . lines <$> readFile fileloc
    let m = iterate step' $ M.fromList $ map (,1) stones
    -- Part 1
    print $ totalStones (m !! 25)
    -- Part 2
    print $ totalStones (m !! 75)
