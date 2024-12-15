import Data.Bifunctor qualified as BF
import Data.List
import Data.Ord
import System.Environment

type Matrix a = [[a]]

swapRows :: Int -> Int -> [a] -> [a]
swapRows i j xs =
    let elemI = xs !! i
        elemJ = xs !! j
        left = take i xs
        middle = take (j - i - 1) (drop (i + 1) xs)
        right = drop (j + 1) xs
     in left ++ [elemJ] ++ middle ++ [elemI] ++ right

gaussianElim :: Matrix Double -> [Double] -> (Matrix Double, [Double])
gaussianElim = gaussianElim' 0

gaussianElim' :: Int -> Matrix Double -> [Double] -> (Matrix Double, [Double])
gaussianElim' _ [] [] = ([], [])
gaussianElim' _ [m] [p] = ([m], [p])
gaussianElim' j mm@(m : ms) pp@(p : ps)
    | pivot == 0 = (mm, pp)
    | otherwise = let (nm, np) = gaussianElim' (j + 1) modM modP in (m' : nm, p' : np)
  where
    (m' : ms', p' : ps') = if m !! j == 0 then getNew else (mm, pp)
    getNew =
        let (bestRow, bestIndex) =
                maximumBy
                    (comparing (abs . (!! j) . fst))
                    (zip ms [0 ..])
         in (swapRows 0 bestIndex mm, swapRows 0 bestIndex pp)
    pivot = m' !! j

    (modM, modP) = unzip $ map eliminateRow (zip3 ms' ps' [0 ..])
    eliminateRow (row, rhs, _) =
        let factor = row !! j / pivot
            newRow = zipWith (-) row (map (* factor) m')
            newRhs = rhs - factor * p'
         in (newRow, newRhs)

backSub :: (Matrix Double, [Double]) -> [Double]
backSub (ms, ps) = foldr solve [] $ zip ms ps
  where
    solve :: ([Double], Double) -> [Double] -> [Double]
    solve (e, p) acc = p' : acc
      where
        m = length e - length acc - 1
        (s : ss) = drop m e
        z = zipWith (*) ss acc
        p' = (p - sum z) / s

parseEquation :: [String] -> (Matrix Double, [Double])
parseEquation [a, b, p] = ([[ax, bx], [ay, by]], [px, py])
  where
    getXY x = (takeWhile (/= ',') x, drop 3 $ dropWhile (/= ',') x)
    parseXY v = let (x : xs, y : ys) = getXY v in (if x == '-' then negate $ read xs else read xs, if y == '-' then negate $ read ys else read ys)
    (ax, ay) = parseXY $ drop 11 a
    (bx, by) = parseXY $ drop 11 b
    (px, py) = parseXY $ drop 8 p

main :: IO ()
main = do
    [fileloc] <- getArgs
    file <- map (filter (not . null)) . groupBy (const (not . null)) . lines <$> readFile fileloc
    let equations = map parseEquation file
    -- Part 1
    print $ findTheThing equations
    -- Part 2
    print $ findTheThing $ map (BF.second (map (+ 10000000000000))) equations
  where
    findTheThing = sum . map ((\[a, b] -> 3 * a + b) . map round) . filter withinRange . map (backSub . uncurry gaussianElim)
    withinRange = all (\v -> abs (fromIntegral (round v) - v) < e)
      where
        e = 0.0001
