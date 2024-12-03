import Data.Char
import Data.Text qualified as T
import System.Environment

data Mul = Mul Int Int deriving (Show)

getVal :: Mul -> Int
getVal (Mul a b) = a * b

getMuls :: String -> [Mul]
getMuls [] = []
getMuls s = m : getMuls nxt
  where
    (m, nxt) = getMuls' s

getMuls' :: String -> (Mul, String)
getMuls' s
    | null n = (Mul 0 0, "")
    | otherwise = case parseMul nxt of
        Nothing -> getMuls' $ drop 1 n
        Just m -> (m, dropWhile (/= ')') n)
  where
    n = dropWhile (/= 'm') s
    nxt = takeWhile (/= ')') n

getDoMuls :: String -> [Mul]
getDoMuls = concatMap (getMuls . (T.unpack . head) . T.splitOn (T.pack "don't()")) . T.splitOn (T.pack "do()") . T.pack

parseMul :: String -> Maybe Mul
parseMul ('m' : 'u' : 'l' : '(' : x)
    | c == ',' && null (dropWhile isNumber secs) = (Mul <$> saveRead fir) <*> saveRead sec
    | otherwise = Nothing
  where
    fir = takeWhile isNumber x
    (c : secs) = dropWhile isNumber x
    sec = takeWhile isNumber secs
    saveRead s = if all isNumber s then Just (read s) else Nothing
parseMul _ = Nothing

main = do
    [fileLoc] <- getArgs
    file <- readFile fileLoc
    -- Part 1
    let muls = getMuls file
    print $ foldr ((+) . getVal) 0 muls

    -- Part 2
    let doMuls = getDoMuls file
    print $ foldr ((+) . getVal) 0 doMuls
