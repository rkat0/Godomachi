module Graph
    (Vertex,Edge,Graph,
    tokyo,america,japan,
    buildDG,
    buildNDG,
    vertices,
    edges,
    edgesND,
    degree,
    eccentricity,
    radCent,
    radius,
    center,
    diamiter,
    isConnected,
    unionfind,
    dfsT,
    dfsF,
    dfsSets,
    dfsWithBack,
    fromTree,
    dfsWithLowlink,
    articulation,
    articulationPartition,
    notArticulation,
    articulationDfsLowlink,
    bridge,
    bridgeDfsLowlink,
    distance,
    distanceSet
    ) where

import Data.Maybe (fromJust)
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Tree
import Data.List ((\\))
import Control.Monad.State

import UnionFind

type Vertex a = a
type Edge a = (Vertex a, Vertex a)
type Graph a = M.Map (Vertex a) [Vertex a]

-- Some toys
tokyo = buildNDG [1..23]
    [(1,2),(1,7),(1,8),(2,3),(2,6),(2,7),(3,4),(3,5),(3,6),(4,5),(4,11),(4,12),(5,6),(5,10),(5,11),
     (6,7),(6,8),(6,9),(6,10),(7,8),(8,9),(8,17),(9,10),(9,15),(9,16),(9,17),(10,11),(10,14),(10,15),
     (11,12),(11,13),(11,14),(12,13),(13,14),(13,21),(13,22),(14,15),(14,20),(14,21),(15,16),(15,20),
     (16,17),(16,19),(16,20),(17,18),(17,19),(18,19),(18,23),(19,20),(19,21),(19,22),(19,23),(20,21),
     (21,22),(22,23)]

japan = buildNDG [1..46]
    [(1,2),(2,3),(2,4),(3,4),(3,5),(3,6),(4,5),(5,6),(5,8),(6,7),(6,8),(7,8),(7,9),(7,18),(7,19),(8,9),
     (8,10),(8,11),(9,10),(9,12),(9,18),(10,11),(10,12),(11,12),(11,13),(12,13),(12,14),(12,17),(12,18),
     (13,14),(13,15),(14,15),(14,17),(15,16),(15,17),(16,17),(16,18),(16,22),(17,18),(18,19),(18,21),
     (18,22),(19,20),(19,21),(20,21),(20,25),(21,22),(21,23),(21,24),(21,25),(22,23),(23,24),(23,26),
     (23,27),(23,28),(24,25),(24,26),(25,26),(26,27),(26,29),(26,30),(27,28),(27,29),(28,29),(29,30),
     (30,31),(30,32),(30,34),(31,32),(31,37),(31,38),(32,33),(32,37),(33,34),(33,36),(34,35),(34,36),
     (35,36),(36,37),(37,38),(37,39),(38,39),(39,40),(40,41),(40,43),(40,44),(41,42),(43,44),(43,45),
     (43,46),(44,45),(45,46)]

america = buildNDG [1..48]
    [(1,2),(2,3),(2,4),(3,4),(3,5),(4,5),(4,6),(4,7),(5,6),(5,8),(5,9),(6,7),(8,9),(8,10),(8,11),
     (8,12),(8,13),(9,13),(10,11),(10,14),(10,15),(10,16),(11,12),(11,16),(11,17),(12,13),(12,17),
     (14,15),(14,18),(15,16),(15,19),(16,17),(16,19),(16,20),(16,21),(17,21),(17,22),(18,19),(18,23),
     (18,24),(19,20),(19,24),(20,21),(20,24),(20,25),(20,26),(20,27),(20,28),(21,22),(21,28),(21,29),
     (21,30),(21,31),(22,31),(22,32),(23,24),(23,34),(23,35),(24,25),(24,35),(25,26),(25,35),(25,36),
     (25,37),(26,27),(26,37),(27,28),(27,37),(27,38),(27,39),(28,29),(28,39),(28,40),(29,30),(29,40),
     (30,31),(30,33),(31,32),(31,33),(34,35),(34,41),(35,36),(35,41),(36,37),(36,41),(36,42),(36,43),
     (37,38),(37,43),(38,39),(38,44),(39,40),(41,42),(42,43),(42,45),(42,46),(42,47),(43,44),(43,47),
     (44,47),(44,48),(45,46),(46,47),(46,48),(47,48)]

buildDG :: Ord a => [Vertex a] -> [Edge a] -> Graph a
buildDG vs es = fmap reverse $ addEdge es $ M.fromSet (const []) (S.fromList vs)
    where  -- must `reverse` to save order of `addEdge` (mainly for dfs)
        addEdge [] g = g
        addEdge ((v1,v2):es) g = addEdge es $ M.adjust (v2:) v1 g

buildNDG :: Ord a => [Vertex a] -> [Edge a] -> Graph a
buildNDG vs es = buildDG vs $ es ++ [(w,v) | (v,w) <- es, v /= w]

vertices :: Ord a => Graph a -> [Vertex a]
vertices = M.keys

edges :: Ord a => Graph a -> [Edge a]
edges g = [(v,w) | v <- vertices g, w <- g M.! v]

edgesND :: Ord a => Graph a -> [Edge a]
edgesND g = [(v,w) | v <- vertices g, w <- g M.! v, v <= w]

degree :: Ord a => Graph a -> Vertex a -> Int
degree g v = length $ g M.! v

eccentricity :: Ord a => Graph a -> Vertex a -> Int
eccentricity g v = eccentricityHelper S.empty [v] g 0
    where
        eccentricityHelper :: Ord a => S.Set a -> [Vertex a] -> Graph a -> Int -> Int
        eccentricityHelper former now g d
            | null next = d - 1
            | otherwise = eccentricityHelper (S.union former $ S.fromList now) next g (d+1)
            where next = filter (`S.notMember` former) $ concat [g M.! v | v <- now]

eccentricityCut :: Ord a => Maybe Int -> Graph a -> Vertex a -> Int
eccentricityCut Nothing g v = eccentricity g v
eccentricityCut (Just n) g v = eccentricityCut' n S.empty [v] g 0
    where
        eccentricityCut' :: Ord a => Int -> S.Set a -> [Vertex a] -> Graph a -> Int -> Int
        eccentricityCut' n former now g d
            | null next = d - 1
            | n < d = d
            | otherwise = eccentricityCut' n (S.union former $ S.fromList now) next g (d+1)
            where next = filter (`S.notMember` former) $ concat [g M.! v | v <- now]

radCent :: Ord a => Graph a -> (Int, [Vertex a])
radCent g = radCentRec Nothing [] (vertices g) g
    where
        radCentRec (Just r) c [] _ = (r,c)
        radCentRec Nothing _ (v:vs) g = radCentRec (Just (eccentricity g v)) [v] vs g
        radCentRec m c (v:vs) g =
            case compare ecc (fromJust m) of
                LT -> radCentRec (Just ecc) [v] vs g
                EQ -> radCentRec m (v:c) vs g
                GT -> radCentRec m c vs g
            where ecc = eccentricityCut m g v

radius :: Ord a => Graph a -> Int
radius = fst . radCent

center :: Ord a => Graph a -> [Vertex a]
center = snd . radCent

diamiter :: Ord a => Graph a -> Int
diamiter g = maximum $ map (eccentricity g) (vertices g)

isConnected :: Ord a => Graph a -> Bool
isConnected = single . unionfind

unionfind :: Ord a => Graph a -> UnionFind a
unionfind g = newUnionList (vertices g) (edgesND g)

dfsT :: Ord a => Graph a -> Vertex a -> Tree a
dfsT g v = prune' $ Node v (map (generate g) (g M.! v))
    where prune' (Node r ts) = Node r (evalState (prune ts) (S.singleton r))

dfsF :: Ord a => Graph a -> Forest a
dfsF g = prune' $ map (generate g) (vertices g)
    where prune' ts = evalState (prune ts) S.empty

dfsSets :: Ord a => Graph a -> Vertex a -> [S.Set a]
dfsSets g r = map (dfsS g r) (g M.! r)
    where
        dfsS :: Ord a => Graph a -> Vertex a -> Vertex a -> S.Set a
        dfsS g r v = S.delete r . prune' $ Node v (map (generate g) (g M.! v))
            where prune' (Node w ts) = execState (prune ts) (S.fromList [r,w])

generate :: Ord a => Graph a -> Vertex a -> Tree a
generate g v = Node v (map (generate g) (g M.! v))

prune :: Ord a => Forest a -> State (S.Set a) (Forest a)
prune [] = return []
prune (Node r ts:us) = do
    s <- get
    if S.member r s
    then prune us
    else do
        put (S.insert r s)
        ts' <- prune ts
        us' <- prune us
        return (Node r ts':us')

dfsWithBack :: Ord a => Graph a -> Vertex a -> (Graph a,Graph a)
dfsWithBack g r = (buildDG vs es,buildDG vs (edges g \\ es'))
    where
        vs = vertices g
        es = edgesFromForest [dfsT g r]
        es' = es ++ [(w,v) | (v,w) <- es, v /= w]

dfsWithLowlink :: Ord a => Graph a -> Vertex a -> (Graph a,M.Map (Vertex a) (Int,Int))
dfsWithLowlink g r = (dfg,ordLowlink r (M.singleton r (0,0)))
    where
        (dfg,bg) = dfsWithBack g r
        ordLowlink v m = M.insert v (n,l') m'
            where
                (n,l) = m M.! v
                vs = dfg M.! v
                m' = foldr (\vt mt -> ordLowlink vt (M.insert vt (M.size m,M.size m) mt)) m vs
                l' = minimum (l : map (fst . (m' M.!)) (bg M.! v) ++ map (snd . (m' M.!)) (dfg M.! v))

fromTree :: Ord a => Tree a -> Graph a
fromTree t = buildDG (flatten t) (edgesFromForest [t])

edgesFromForest :: Forest a -> [Edge a]
edgesFromForest [] = []
edgesFromForest (Node v ts : us) = [(v,w) | w <- map rootLabel ts] ++ edgesFromForest ts ++ edgesFromForest us

articulationPartition :: Ord a => Graph a -> ([Vertex a],[Vertex a])
articulationPartition g = (arts,vs \\ arts)
    where
        arts = articulationDfsLowlink g r (dfsWithLowlink g r)
        vs = vertices g
        r = head vs

articulation :: Ord a => Graph a -> [Vertex a]
articulation = fst . articulationPartition

notArticulation :: Ord a => Graph a -> [Vertex a]
notArticulation = snd . articulationPartition

articulationDfsLowlink :: Ord a => Graph a -> Vertex a -> (Graph a,M.Map (Vertex a) (Int,Int)) -> [Vertex a]
articulationDfsLowlink g r (dfg,m) = if length (dfg M.! r) == 1 then arts else r:arts
    where
        vs = vertices g \\ [r]
        arts = filter isArt vs
        isArt v = any (\w -> vo <= snd (m M.! w)) (dfg M.! v)
            where vo = fst (m M.! v)

bridge :: Ord a => Graph a -> [Edge a]
bridge g = bridgeDfsLowlink $ dfsWithLowlink g (head $ vertices g)

bridgeDfsLowlink :: Ord a => (Graph a,M.Map (Vertex a) (Int,Int)) -> [Edge a]
bridgeDfsLowlink (dfg,m) = filter isBridge (edges dfg)
    where
        isBridge (u,v) = fst (m M.! u) < snd (m M.! v)

distance :: Ord a => Graph a -> Vertex a -> Vertex a -> Int
distance g v w = distanceSet g (S.singleton v) (S.singleton w)

distanceSet :: Ord a => Graph a -> S.Set (Vertex a) -> S.Set (Vertex a) -> Int
distanceSet g = distance' 0
    where
        distance' d vs ws
            | S.disjoint vs ws =
                if S.size vs <= S.size ws
                then distance' (d+1) (update vs) ws
                else distance' (d+1) vs (update ws)
            | otherwise = d
        update vs = S.union (S.fromList $ concatMap (g M.!) (S.toList vs)) vs
