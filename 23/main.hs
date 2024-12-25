import Data.List
import Data.Map qualified as M
import Data.Set qualified as S
import System.Environment

parseConnections :: [String] -> M.Map String (S.Set String)
parseConnections [] = M.empty
parseConnections (c : cs) = M.unionWith S.union (M.fromList [(f, S.singleton t), (t, S.singleton f)]) $ parseConnections cs
  where
    (f, t) = (takeWhile (/= '-') c, drop 1 $ dropWhile (/= '-') c)

makeTrios :: M.Map String (S.Set (String, String)) -> S.Set (S.Set String)
makeTrios = foldr (S.union . (\(k, v) -> S.map (\(a, b) -> S.fromList [k, a, b]) v)) S.empty . M.assocs

pairUp :: (Ord k) => M.Map k (S.Set k) -> M.Map k (S.Set (k, k))
pairUp m = M.map (\v -> S.filter (\(a, b) -> b `S.member` (m M.! a) && a `S.member` (m M.! b)) $ S.cartesianProduct v v) m

main :: IO ()
main = do
    [fileLoc] <- getArgs
    cons <- parseConnections . lines <$> readFile fileLoc
    let paired = pairUp cons
    -- Part 1
    print $ length $ S.filter (any ((== 't') . head) . S.toList) $ makeTrios paired
    -- Part 2
    let t = map (\(k, v) -> S.unions $ S.map (\(a, b) -> S.fromList [k, a, b]) v) $ M.assocs paired
    putStrLn $ intercalate "," $ S.toList $ fst $ maximumBy (\(_, a) (_, b) -> compare a b) $ M.assocs $ M.fromListWith (+) $ map (,1) t
