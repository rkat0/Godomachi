import System.Environment (getArgs)
import System.IO
import Control.Monad.State
import Control.Arrow
import Data.List (sort,(\\))
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.IntMap as IM
import qualified Data.Map as M
import Data.Tree

import Graph

type Coord = Int
type Point = (Coord, Coord)
type Polyomino = [Point]
type Cut = (Polyomino,Polyomino)
type Board = (Polyomino,Polyomino)
type Move = (Polyomino,Polyomino,Polyomino)

-- Finds the min x and y coordiate of a Polyomino.
minima :: Polyomino -> Point
minima (p:ps) = foldr (\(x, y) (mx, my) -> (min x mx, min y my)) p ps

translateToOrigin :: Polyomino -> Polyomino
translateToOrigin p =
    let (minx, miny) = minima p in
        map (\(x, y) -> (x - minx, y - miny)) p

rotate90, rotate180, rotate270, reflect :: Point -> Point
rotate90  (x, y) = ( y, -x)
rotate180 (x, y) = (-x, -y)
rotate270 (x, y) = (-y,  x)
reflect   (x, y) = (-x,  y)

-- All the plane symmetries of a rectangular region.
rotationsAndReflections :: Polyomino -> [Polyomino]
rotationsAndReflections p =
    [p,
     map rotate90 p,
     map rotate180 p,
     map rotate270 p,
     map reflect p,
     map (rotate90 . reflect) p,
     map (rotate180 . reflect) p,
     map (rotate270 . reflect) p]

canonical :: Polyomino -> Polyomino
canonical = minimum . map (sort . translateToOrigin) . rotationsAndReflections

equiv :: Polyomino -> Polyomino -> Bool
equiv p q = canonical p == canonical q

unique :: (Ord a) => [a] -> [a]
unique = S.elems . S.fromList

-- All four points in Von Neumann neighborhood.
contiguous :: Point -> [Point]
contiguous (x, y) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

-- Finds all distinct points that can be added to a Polyomino.
newPoints :: Polyomino -> [Point]
newPoints p =
    let notInP = filter (not . flip elem p) in
        unique . notInP . concatMap contiguous $ p

newPolys :: Polyomino -> [Polyomino]
newPolys p = unique . map (canonical . flip (:) p) $ newPoints p

monomino = [(0, 0)]
monominoes = [monomino]

-- Generates polyominoes of rank n recursively.
rank :: Int -> [Polyomino]
rank 0 = []
rank 1 = monominoes
rank n = unique . concatMap newPolys $ rank (n - 1)

-- Generates a textual representation of a Polyomino.
textRepresentaton :: Polyomino -> String
textRepresentaton p
    | null p = []
    | otherwise = unlines [[if (x,y) `elem` p then '#' else '.' | x <- [0 .. maxx]] | y <- [0 .. maxy]]
    where
        maxima :: Polyomino -> Point
        maxima (p:ps) = foldr (\(x, y) (mx, my) -> (max x mx, max y my)) p ps
        (minx, miny) = minima p
        (maxx, maxy) = maxima p

fromString :: [String] -> Polyomino
fromString [] = errorWithoutStackTrace "fromString: empty input"
fromString ss =  read2 0 ss
    where
        read2 _ [] = []
        read2 y (s:ss) = read1 0 y s ++ read2 (y+1) ss
        read1 _ _ [] = []
        read1 x y (c:s) = if c == '#' then (x,y):read1 (x+1) y s else read1 (x+1) y s

countPolyominoes :: Int -> Int
countPolyominoes = length . rank

printP :: Polyomino -> IO()
printP = putStrLn . textRepresentaton

printPolyominoes :: Int -> IO()
printPolyominoes n = do
    let ps = rank n
    putStrLn $ "\nAll free " ++ show n ++ "-ominoes : " ++ show (length ps)
    mapM_ (\(i,p) -> print i >> putStrLn (textRepresentaton p)) $ zip [0..] ps

polyToGraph :: Polyomino -> Graph Point
polyToGraph p = buildNDG p es
    where es = concatMap (\(x,y) -> filter (\(_,z) -> elem z p) [((x,y),(x+1,y)),((x,y),(x,y+1))]) p

isValid :: Polyomino -> Bool
isValid = isConnected . polyToGraph

cutWithCan :: Polyomino -> Polyomino -> Cut
cutWithCan p a = (canonical a,canonical (p \\ a))

cutWith :: Polyomino -> Polyomino -> Cut
cutWith p a = (a,p \\ a)

cut1 :: Polyomino -> [Cut]
cut1 = unique . map (canonical *** canonical) . cut1'

cut1' :: Polyomino -> [Cut]
cut1' p = unique $ map (cutWith p) narts
    where
        narts = map (:[]) $ notArticulation $ polyToGraph p

cutWithEx :: Polyomino -> Int -> ([[Cut]],[Cut])
cutWithEx p 1 = ([cut1' p],[])
cutWithEx p n = (cs' : css, exr ++ ex_big)
    where
        cs' = (exn ++) . concat . zipWith add1 cs1 $ map f cs1
        exr = concat . zipWith cutExPoints cs1 $ map g cs1
        (exn,ex_big) = L.partition (\(x,_) -> length x == n) ex
        (css@(cs1:_),ex) = cutWithEx p (n-1)
        add1 (a,b) = foldr (\x -> (:) (x:a,L.delete x b)) []
        f (a,b) = newPoints a `L.intersect` notArticulation (polyToGraph b)
        g (a,b) = newPoints a `L.intersect` articulation (polyToGraph b)

cutExPoints :: Cut -> [Point] -> [Cut]
cutExPoints (c,poly) = map minCut . filter f . map connectBlocks
    where
        pg = polyToGraph poly
        connectBlocks = L.sortBy (\x y -> compare (S.size y) (S.size x)) . S.toList . S.fromList . dfsSets pg
        f x = S.size (head x) * 2 >= length c + length poly
        minCut = (\b -> (sort $ c ++ (poly \\ b),b)) . S.elems . head

cutAllEx :: Polyomino -> [[Cut]]
cutAllEx p = map (L.nubBy (\(a,b) (c,d) -> a == c && equiv b d) . map (first canonical)) . fst $ cutWithEx p (length p `div` 2)

searchPrint :: Polyomino -> Polyomino -> IO()
searchPrint p q = do
    divide
    printP p
    printP q
    printMoves match1
    where
        match1 = concatMap (filter (\(_,c1,c2) -> canonical c1 == canonical c2)) (move (p,q))

divide = putStrLn (replicate 10 '-' ++ "\n")

printMove :: Move -> IO()
printMove (c,p1,p2) = printP c >> printP p1 >> printP p2

printMoves :: [Move] -> IO()
printMoves ms = divide >> mapM_ (\m -> printMove m >> divide) ms

move :: (Polyomino,Polyomino) -> [[Move]]
move (p,q) =
    if length p /= length q
    then errorWithoutStackTrace "move: different size poliominoes"
    else map (\(xs,ys) -> [(c,c1,c2) | (c,c1) <- xs, (c',c2) <- ys, c == c']) $ zip cp cq
    where
        cp = cutAllEx p
        cq = cutAllEx q

isEnd :: Board -> Bool
isEnd (p1,p2) = equiv p1 p2

isEnd' :: Move -> Bool
isEnd' (_,p1,p2) = equiv p1 p2

search :: Tree Move -> [Move]
search tree@(Node m@(_,p1,p2) _)
    | equiv p1 p2 = [m]
    | otherwise = reverse $ evalState (search1 $ prune tree) (1,- scoreMax,[m],[m])
    where
        scoreMax = length p1
        search1 :: Tree Move -> State (Int,Int,[Move],[Move]) [Move]
        search1 (Node b ts) = do
            eval1' ts
            (_,_,best,_) <- get
            return best
            where
                eval1' :: [Tree Move] -> State (Int,Int,[Move],[Move]) [Move]
                eval1' [] = get >>= (\(_,_,_,now) -> return now)
                eval1' moves = do
                    let step1 = filter (\(Node _ ts) -> null ts) moves
                    if null step1
                    then eval1 moves
                    else do
                        let (Node m _) = head step1
                        (n,s,best,now) <- get
                        let next = m:now
                        let s' = scoreMax - n
                        put (n,s',next,now) >> return now

                eval1 :: [Tree Move] -> State (Int,Int,[Move],[Move]) [Move]
                eval1 [] = get >>= (\(_,_,_,now) -> return now)
                eval1 (Node r ts : rest) = do
                    (n,s,best,now) <- get
                    let next = r:now
                    let s' = scoreMax - n
                    if s' - 2 <= s
                    then return now
                    else do
                        put (n+1,scoreMax,[],next)
                        eval2' ts
                        get >>= (\(_,s',best',_) -> put $ if s <= s' then (n,s',best',now) else (n,s,best,now))
                        eval1 rest

                eval2' :: [Tree Move] -> State (Int,Int,[Move],[Move]) [Move]
                eval2' [] = get >>= (\(_,_,_,now) -> return now)
                eval2' moves = do
                    let step1 = filter (\(Node _ ts) -> null ts) moves
                    if null step1
                    then eval2 moves
                    else do
                        let (Node m _) = head step1
                        (n,s,best,now) <- get
                        let next = m:now
                        let s' = n - scoreMax
                        put (n,s',next,now) >> return now

                eval2 :: [Tree Move] -> State (Int,Int,[Move],[Move]) [Move]
                eval2 [] = get >>= (\(_,_,_,now) -> return now)
                eval2 (Node r ts : rest) = do
                    (n,s,best,now) <- get
                    let next = r:now
                    let s' = n - scoreMax
                    if s' + 2 >= s
                    then return now
                    else do
                        put (n+1,- scoreMax,[],next)
                        eval1' ts
                        get >>= (\(_,s',best',_) -> put $ if s >= s' then (n,s',best',now) else (n,s,best,now))
                        eval2 rest

firstMove :: Board -> (Int,[[Move]])
firstMove b = (length moves,filter (odd . length) moves)
    where
        moves = map search . subForest $ gametree b

printFirstMoves :: Board -> IO()
printFirstMoves b@(p1,p2) = do
    let (num,mss) = firstMove b
    divide
    putStrLn "Initial board\n"
    printP p1
    printP p2
    mapM_ (\xs@(x:_) -> divide >> putStrLn (show (length xs) ++ " steps\n") >> printMove x) mss
    putStrLn $ "first wins in " ++ show (length mss) ++ '/':show num ++ " patterns"

prune :: Tree Move -> Tree Move
prune (Node r ts) = Node r (if isEnd' r then [] else map prune ts)

gametree :: Board -> Tree Move
gametree (p1,p2) = reptree (concat . move . (\(_,x,y) -> (x,y))) ([],p1,p2)
    where
        reptree f b = Node b (map (reptree f) (f b))

solve :: (Polyomino,Polyomino) -> IO()
solve = printAns . search . gametree
    where
        printAns ms = do
            printMoves ms
            putStr $ (if odd step then "First" else "Second") ++ " player wins in "
            putStr $ show step
            putStrLn " step!"
            where step = length ms - 1

solveAll :: Int -> IO()
solveAll n = do
    putStrLn $ show n ++ "-ominoes : " ++ show m
    putStrLn $ "total patterns : " ++ show total ++ " (= " ++ show m ++ " * " ++ show (m-1) ++ " / 2)"
    putStrLn $ "        first  : " ++ show (total - lose)
    putStrLn $ "                 " ++ show (IM.toAscList win1)
    putStrLn $ "        second : " ++ show lose
    putStrLn $ "                 " ++ show (IM.toAscList win2)
--    mapM_ (\(p1,p2) -> divide >> printP p1 >> printP p2 >> divide) lose' -- list all "lose" ptterns
    where
        lose = IM.foldr (+) 0 win2
        (win1,win2) = IM.partitionWithKey (\k _ -> odd k) moveCount
        moveCount = IM.fromListWith (+) $ map (\x -> (length x-1,1)) moves
        moves = map (search . gametree) [(p1,p2) | p1 <- ps, p2 <- ps, p1 < p2]
        total = m*(m-1) `div` 2
        m = length ps
        ps = rank n

solveAllMat :: Int -> IO()
solveAllMat n = do
    putStrLn $ show n ++ "-ominoes : " ++ show m
    putStrLn $ "total patterns : " ++ show total ++ " (= " ++ show m ++ " * " ++ show (m-1) ++ " / 2)"
    putStrLn $ "        first  : " ++ show (total - lose)
    putStrLn $ "                 " ++ show (IM.toAscList win1)
    putStrLn $ "        second : " ++ show lose
    putStrLn $ "                 " ++ show (IM.toAscList win2)
    putStrLn $ unlines [[find (i,j) | j <- [1..m]] | i <- [1..m]]
    where
        lose = IM.foldr (+) 0 win2
        (win1,win2) = IM.partitionWithKey (\k _ -> odd k) moveCount
        moveCount = IM.fromListWith (+) $ map (\(_,x) -> (x,1)) moves
        moveMap = M.fromList moves
        moves = map (second (subtract 1 . length . search . gametree)) [((i1,i2),(p1,p2)) | (i1,p1) <- zip [1..] ps, (i2,p2) <- zip [1..] ps, i1 < i2]
        total = m*(m-1) `div` 2
        m = length ps
        ps = rank n
        find (i,j)
            | i == j = '0'
            | i > j = head . show $ moveMap M.! (j,i)
            | i < j = head . show $ moveMap M.! (i,j)

getLines :: IO [String]
getLines = do
    s <- getLine
    if null s then return [] else fmap (s:) getLines

main = do
    hSetBuffering stdout LineBuffering
    args <- getArgs
    if null args
    then do
        p1 <- fromString <$> getLines
        p2 <- fromString <$> getLines
        solve (p1,p2)
    else if head args == "--first"
    then do
        p1 <- fromString <$> getLines
        p2 <- fromString <$> getLines
        printFirstMoves (p1,p2)
    else if head args == "--rank"
    then printPolyominoes (read $ args !! 1)
    else if head args == "--matrix"
    then solveAllMat . read $ args !! 1
{-
    else if head args == "--custom"
    then do
        let p8 = rank 8
        tab <- lines <$> readFile "8unit"
        let idx = filter (uncurry (<)) . map ((\x -> (x`div`369,x`mod`369)) . fst) . filter (\(_,n) -> n=='1') $ zip [0..] (concat tab)
        let pp = map ((!!) p8 *** (!!) p8) idx
        let pp_diff = filter ((== 6) . length . search . gametree) pp
        mapM_ (\(p,q) -> divide >> printP p >> printP q >> divide ) pp_diff
-}
    else solveAll . read . head $ args
