import Control.Applicative
import Data.Bifunctor qualified as BF
import Data.List
import Data.Map qualified as M
import System.Environment

type Loc = (Int, Int)
type Store = M.Map Char [Loc]
data Dir = U | R | D | L deriving (Eq, Show, Ord)

isWithin :: Loc -> Loc -> Bool
isWithin (a, b) (x, y) = (h == 1 && w == 0) || (w == 1 && h == 0)
  where
    h = abs (b - y)
    w = abs (a - x)

subGrouping :: [Loc] -> [[Loc]]
subGrouping = subGrouping' []
  where
    subGrouping' :: [[Loc]] -> [Loc] -> [[Loc]]
    subGrouping' p [] = p
    subGrouping' p (x : xs) = case filter (any (isWithin x)) p of
        [] -> subGrouping' ([x] : p) xs
        [i] -> let p' = delete i p in subGrouping' ((x : i) : p') xs
        l -> let p' = p \\ l in subGrouping' ((x : concat l) : p') xs

perimeter :: [Loc] -> [(Loc, Dir)]
perimeter = perimeter' []
  where
    perimeter' :: [Loc] -> [Loc] -> [(Loc, Dir)]
    perimeter' p [] = []
    perimeter' p (loc@(x, y) : xs) = map ((loc,) . snd) (filter (not . fst) [(right, R), (left, L), (up, U), (down, D)]) ++ perimeter' (loc : p) xs
      where
        l = p ++ xs
        right = any (\(a, b) -> b == y && a - x == 1) l
        left = any (\(a, b) -> b == y && x - a == 1) l
        up = any (\(b, a) -> b == x && a - y == 1) l
        down = any (\(b, a) -> b == x && y - a == 1) l

sides :: [(Loc, Dir)] -> Int
sides l = length $ left ++ right ++ up ++ down
  where
    l' :: M.Map Dir [Loc]
    l' = foldr (\(loc, d) acc -> M.alter (f loc) d acc) M.empty $ nub l
      where
        f l x = fmap (l :) x <|> return [l]
    up = groupedSides [] (l' M.! U) (\(a, b) (x, y) -> b == y && abs (x - a) == 1)
    down = groupedSides [] (l' M.! D) (\(a, b) (x, y) -> b == y && abs (a - x) == 1)
    left = groupedSides [] (l' M.! L) (\(a, b) (x, y) -> a == x && abs (b - y) == 1)
    right = groupedSides [] (l' M.! R) (\(a, b) (x, y) -> a == x && abs (y - b) == 1)
    groupedSides s [] _ = s
    groupedSides s (x : xs) pred = case findIndices (any (pred x)) s of
        [] -> groupedSides ([x] : s) xs pred
        [i] -> let (a, v : b) = splitAt i s in groupedSides (a ++ (x : v) : b) xs pred
        a ->
            let this = concatMap (s !!) a
                rest = foldr (\i acc -> let (a, _ : b) = splitAt i acc in (a ++ b) ++ acc) s $ sort a
             in groupedSides ((x : this) : rest) xs pred

main :: IO ()
main = do
    [fileloc] <- getArgs
    file <- lines <$> readFile fileloc
    let grid = concat $ zipWith (\s l -> zipWith (\a c -> (a, (l, c))) s [0 ..]) file [0 ..]
        mapStore :: Store
        mapStore = foldr (\(x, l) acc -> M.alter (\i -> fmap (l :) i <|> return [l]) x acc) M.empty grid
    let res = map (\a -> (length a, perimeter a)) $ concatMap (subGrouping . sortOn snd) $ M.elems mapStore
    -- Part 1
    print $ sum $ map (uncurry (*) . BF.second length) res
    -- Part 2
    print $ sum $ map (uncurry (*) . BF.second sides) res
