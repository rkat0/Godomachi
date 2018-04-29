module UnionFind
    (newUnionFind,
    newFresh,
    newFreshList,
    newUnionList,
    fresh,
    freshList,
    union,
    unionList,
    single,
    UnionFind,
    Point
    ) where

import qualified Data.IntMap as IM
import Data.List (elemIndex)
import Control.Arrow

data UnionFind a = UnionFind Int (IM.IntMap (Link a))
    deriving (Show)

data Link a = Info Int a | Link Int
    deriving (Show)

newtype Point a = Point Int

newUnionFind :: UnionFind a
newUnionFind = UnionFind 0 IM.empty

newFresh :: a -> (UnionFind a, Point a)
newFresh = fresh newUnionFind

newFreshList :: [a] -> (UnionFind a, [Point a])
newFreshList = freshList newUnionFind

newUnionList :: Eq a => [a] -> [(a,a)] -> UnionFind a
newUnionList vs es = unionList uf es'
    where
        (uf,_) = newFreshList vs
        es' = map (convert *** convert) es
        convert x = case elemIndex x vs of
            Just n -> Point n
            Nothing -> errorWithoutStackTrace "newUnionList: vertex not found"

fresh :: UnionFind a -> a -> (UnionFind a, Point a)
fresh (UnionFind next im) a = (UnionFind (next+1) (IM.insert next (Info 0 a) im), Point next)

freshList :: UnionFind a -> [a] -> (UnionFind a, [Point a])
freshList uf as = freshList' uf as []
    where
        freshList' uf [] ps = (uf, reverse ps)
        freshList' uf (a:as) ps = let (uf',p) = fresh uf a in freshList' uf' as (p:ps)

union :: UnionFind a -> Point a -> Point a -> UnionFind a
union uf@(UnionFind next im) p1 p2 =
    apply uf p1 $ \i1 r1 _ ->
    apply uf p2 $ \i2 r2 a2 ->
    if i1 == i2 then uf else case compare r1 r2 of
        LT -> let im1 = IM.insert i1 (Link i2) im in UnionFind next im1
        EQ -> let
            im1 = IM.insert i1 (Link i2) im
            im2 = IM.insert i2 (Info (r2+1) a2) im1 in UnionFind next im2
        GT -> let
            im1 = IM.insert i1 (Info r2 a2) im
            im2 = IM.insert i2 (Link i1) im1 in UnionFind next im2

unionList :: UnionFind a -> [(Point a,Point a)] -> UnionFind a
unionList uf [] = uf
unionList uf ((p1,p2):ps) = let uf' = union uf p1 p2 in unionList uf' ps

apply :: UnionFind a -> Point a -> (Int -> Int -> a -> b) -> b
apply (UnionFind _ im) (Point n) f = find n
    where
        find i = case im IM.! i of
            Link i' -> find i'
            Info r a -> f i r a

nUnions :: UnionFind a -> Int
nUnions (UnionFind _ im) = IM.size (IM.filter isRepr im)
    where
        isRepr (Link _) = False
        isRepr (Info _ _) = True

single :: UnionFind a -> Bool
single uf = nUnions uf == 1
