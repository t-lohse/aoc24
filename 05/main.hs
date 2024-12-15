import Data.List
import System.Environment

type Rule = (Int, Int)
type Update = [Int]

parseRules :: [String] -> [Rule]
parseRules = map ((,) <$> read . takeWhile (/= '|') <*> read . drop 1 . dropWhile (/= '|'))

parseUpdates :: [String] -> [Update]
parseUpdates = map (read . (++ "]") . ('[' :))

correctlyOrdered :: [Rule] -> Update -> Bool
correctlyOrdered r u = not $ any test r
  where
    test (a, b) = case (elemIndex a u, elemIndex b u) of
        (Just a', Just b') -> a' > b'
        _ -> False

correctUpdate :: [Rule] -> Int -> Int -> Ordering
correctUpdate r a b
    | any (\(a', b') -> a' == a && b' == b) r = LT
    | any (\(a', b') -> a' == b && b' == a) r = GT
    | otherwise = EQ

main = do
    [fileLoc] <- getArgs
    file <- lines <$> readFile fileLoc

    -- Part 1
    let (rules', _ : updates') = break null file
        rules = parseRules rules'
        updates = parseUpdates updates'
        filtered = filter (correctlyOrdered rules) updates
        sumMid = sum . map (\s -> s !! (length s `div` 2))
    print $ sumMid filtered
    -- Part 2
    let wrongs = filter (not . correctlyOrdered rules) updates
        sorted = map (sortBy (correctUpdate rules)) wrongs
    print $ sumMid sorted
