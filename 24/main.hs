import Control.Monad.ST
import Data.Bits
import Data.List
import Data.Map qualified as M
import Data.STRef
import Data.Set qualified as S
import Language.Haskell.TH.PprLib (double)
import System.Environment

type Var = String
type Val = Bool

data Op = AND | OR | XOR deriving (Show, Eq)

data Gate = Gate
    { input :: (Var, Var)
    , operation :: Op -- Val -> Val -> Val
    , output :: Var
    }
    deriving (Show, Eq)

getOperation :: Op -> (Val -> Val -> Val)
getOperation AND = (&&)
getOperation OR = (||)
getOperation XOR = (/=)

parseProg :: [String] -> (M.Map Var Val, [Gate])
parseProg s = (M.fromList vars', prog')
  where
    vars = takeWhile (not . null) s
    vars' = map (\s' -> (takeWhile (/= ':') s', (== 1) $ read $ drop 1 $ dropWhile (/= ':') s')) vars
    prog = drop 1 $ dropWhile (not . null) s
    prog' = map parseProg prog
    parseProg s = Gate i op o
      where
        s' = words s
        i = (head s', s' !! 2)
        o = s' !! 4
        op = case s' !! 1 of
            "AND" -> AND
            "OR" -> OR
            "XOR" -> XOR
            _ -> error $ "Not a valid operation: " ++ s' !! 1

evalGates :: STRef s (M.Map Var Val) -> [Gate] -> ST s ()
evalGates ma gates = do
    m <- readSTRef ma
    case find (\g -> let (a, b) = input g in a `M.member` m && b `M.member` m && output g `M.notMember` m) gates of
        Just g ->
            let (a', b') = input g
                (a, b) = (m M.! a', m M.! b')
                o = output g
                op = getOperation $ operation g
             in do
                    writeSTRef ma $ M.insert o (op a b) m
                    evalGates ma gates
        Nothing -> pure ()

{- TOT: Search for full adder
 - CAN BE INVERSED WTF - VERIFY
    Z_i  = (X'_I) XOR C_(i-1)
    C_i  = (A_I) OR (X'_I AND C_(i-1))
    A_I  = X_I AND Y_I
    X'_I = X_I XOR Y_I
    Maybe start backwards:
    - Find end z, track to get intermediate variables
    - print and find err
    Or start from begining, maintaing the C_i

    cap is n = 45

    function that creates a map over z-vals, and values are the expected gates??
    function that takes the z-gates, and checks whether it holds? (predicate)
    function that constructs the C_i values (var-names)? Then check where no fit
    function that constructs the adder-trees?

    z: lhs = X' --- rhs = A
 -}

data AdderTree a = Node a Op (AdderTree a) (AdderTree a) | Leaf a

-- (What is, What should be)
findNotAdders :: STRef s (M.Map Var Val) -> ST s [(Var, Var)]
findNotAdders ma = do
    m <- readSTRef ma
    case "z00" `M.lookup` m of
        Nothing -> pure []
        Just val -> findNotAdders' ma 1 "this"
  where
    findNotAdders' :: STRef s (M.Map Var Val) -> Int -> Var -> ST s [(Var, Var)]
    findNotAdders' ma i cPrev = do
        m <- readSTRef ma
        case ('z' : padLeft 2 '0' (show i)) `M.lookup` m of
            Nothing -> pure []
            Just val -> findNotAdders' ma (i + 1) "this"

padLeft :: Int -> a -> [a] -> [a]
padLeft i c v = replicate (i - length v) c ++ v
padRight :: Int -> a -> [a] -> [a]
padRight i c v = v ++ replicate (i - length v) c

graphviz :: [Var] -> [Gate] -> ([String], [String])
graphviz vert edges = ((map var vert), (graphviz' 0 edges))
  where
    var x = show x ++ " [shape=\"oval\" label=" ++ show x ++ "];"
    graphviz' _ [] = []
    graphviz' i ((Gate (i1, i2) g o) : xs) = [show i1 ++ " -> " ++ boxName ++ ";", show i2 ++ " -> " ++ boxName ++ ";", boxName ++ " -> " ++ show o ++ ";"] ++ graphviz' (i + 1) xs
      where
        box = boxName ++ "[shape=\"box\" label=" ++ show g ++ "]\""
        boxName = show g ++ show i

makeGraphviz :: ([String], [String]) -> String
makeGraphviz (v, e) = "digraph {" ++ v' ++ e' ++ "}"
  where
    v' = intercalate "\n" v
    e' = intercalate "\n" e

simulate :: Int -> Int -> Int -> [Gate] -> Int
simulate c x y prog = runST $ do
    m <- newSTRef $ x' `M.union` y'
    evalGates m prog
    m' <- readSTRef m
    return $ foldr ((\x acc -> x .|. (acc `shiftL` 1)) . (\(_, v) -> if v then 1 else 0)) 0 (filter ((== 'z') . head . fst) $ M.assocs m')
  where
    x' = M.fromList $ zipWith (\i v -> ('x' : padLeft 2 '0' (show i), v == 1)) [0 ..] (reverse $ padLeft c 0 $ conv x)
    y' = M.fromList $ zipWith (\i v -> ('y' : padLeft 2 '0' (show i), v == 1)) [0 ..] (reverse $ padLeft c 0 $ conv y)
    conv 0 = []
    conv i = conv (i `shiftR` 1) ++ [i .&. 1]

-- where var x = show x ++ " [shape=\"oval\" label=\"" ++ show x ++ "\"];"
-- X39
main :: IO ()
main = do
    [fileLoc] <- getArgs
    (init, prog) <- parseProg . lines <$> readFile fileLoc
    print init
    let res = runST $ do
            m <- newSTRef init
            evalGates m prog
            readSTRef m
    print res
    -- Part 1
    let out :: Int
        out = foldr ((\x acc -> x .|. (acc `shiftL` 1)) . (\(_, v) -> if v then 1 else 0)) 0 (filter ((== 'z') . head . fst) $ M.assocs res)
    print out
    -- Part 2
    let relevantGates = filter (\((i1 : i1s), (i2 : i2s), (o : os)) -> i1 == 'x' || i2 == 'y' || o == 'z') $ map (\(Gate ((i1), (i2)) _ (o)) -> (i1, i2, o)) prog
    -- let relevantGates = filter (\((i1 : i1s), (i2 : i2s), (o : os)) -> i1 `elem` "xyz" && i2 `elem` "xyz" && o `elem` "xyz") $ map (\(Gate ((i1), (i2)) _ (o)) -> (i1, i2, o)) prog
    -- let relevantGates = map (\(Gate ((i1), (i2)) _ (o)) -> (i1, i2, o)) prog
    let relevantGates' = filter (\(Gate ((i1 : i1s), (i2 : i2s)) g (o : os)) -> g == AND && i1s == i2s && (i1 == 'x' && i2 == 'y')) prog
    print relevantGates
    print $ filter (\((_ : i1), (_ : i2), (o)) -> i1 == i2 && i1 /= o) relevantGates
    print $ filter (\(Gate ((_ : i1), (_ : i2)) g (o)) -> i1 == i2 && i1 /= o) relevantGates'
    print $ map output relevantGates'
    print $ filter (\(Gate ((i1 : i1s), (i2 : i2s)) g (o : os)) -> o == 'z') prog
    print $ graphviz (M.keys res) prog
    putStrLn $ makeGraphviz $ graphviz (M.keys res) prog
    print $ simulate 44 1 7 prog
    print $ filter (\i -> i + 1 /= simulate 44 1 i prog) [0 .. 2 ^ 45] -- Solve in hands :(
    -- print $ filter (\(i, i') -> i + i' /= simulate 44 i' i prog) $ [(x, y) | x <- [0 .. 44], y <- [0 .. 44]]
    print $ map (\i -> simulate 44 1 i prog) [31 .. 44]
  where
    toVar (v : vs) = (v, read vs)
