import Control.Monad
import Control.Monad.ST
import Data.List
import Data.Map qualified as M
import Data.STRef
import Data.Set qualified as S
import System.Environment

parseStorage :: String -> S.Set String
parseStorage s = S.fromList $ map (filter (/= ',')) $ words s

type Memo = M.Map String Int

canDesignM' :: STRef s (S.Set String) -> STRef s Memo -> String -> ST s Int
canDesignM' storeRef memoRef "" = return 1
canDesignM' storeRef memoRef str = do
    memo <- readSTRef memoRef
    case M.lookup str memo of
        Just result -> return result
        Nothing -> do
            store <- readSTRef storeRef
            b <- newSTRef 0
            let matching = S.filter (`isPrefixOf` str) store
            forM_ matching $ \m -> do
                b' <- readSTRef b
                result <- canDesignM' storeRef memoRef (str \\ m)
                writeSTRef b $ result + b'
            result <- readSTRef b
            modifySTRef memoRef (M.insert str result)
            return result

main :: IO ()
main = do
    [fileLoc] <- getArgs
    ([storeRaw], queries) <- (\s -> (takeWhile (not . null) s, drop 1 $ dropWhile (not . null) s)) . lines <$> readFile fileLoc
    let storage = parseStorage storeRaw
        results = runST $ do
            storeRef <- newSTRef storage
            memoRef <- newSTRef M.empty
            forM queries $ \query -> canDesignM' storeRef memoRef query
    -- Part 1
    print $ length $ filter (> 0) results
    -- Part 2
    print $ sum results
