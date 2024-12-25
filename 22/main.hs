import Control.Monad
import Control.Monad.ST
import Data.Bits
import Data.Map qualified as M
import Data.STRef
import System.Environment

generateSecrets :: Int -> [Int]
generateSecrets n = n : generateSecrets n3
  where
    n1 = prune $ mix n (n * 64)
    n2 = prune $ mix n1 $ floor $ fromIntegral n1 / 32
    n3 = prune $ mix n2 $ n2 * 2048
    mix x v = x `xor` v
    prune x = x `mod` 16777216

type Sequence a = (a, a, a, a)

getMostBananas :: [[Int]] -> Int
getMostBananas l = runST $ do
    acc <- newSTRef M.empty -- M.Map (Sequence Int) Int
    forM_ l $ \secretNums -> do
        acc' <- readSTRef acc
        let seq = getSequences $! map (`mod` 10) secretNums
        writeSTRef acc $! M.unionWith (+) acc' seq
    maximum . M.elems <$> readSTRef acc
  where
    getSequences :: [Int] -> M.Map (Sequence Int) Int
    getSequences (iv : rest@(iii : ii : i : v : xs)) = M.insertWith const (iii - iv, ii - iii, i - ii, v - i) v $! getSequences rest
    getSequences _ = M.empty

main :: IO ()
main = do
    [fileLoc] <- getArgs
    numbers <- map read . lines <$> readFile fileLoc
    -- Part 1
    let mapped = map (take 2001 . generateSecrets) numbers
    print $ sum $! map last mapped
    -- Part 2
    print $! getMostBananas $! mapped
