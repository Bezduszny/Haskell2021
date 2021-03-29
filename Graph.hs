module Graph where
import Set(Set)
import qualified Set as Set
import Data.List (sort)
class Graph g where
  empty   :: g a
  vertex  :: a -> g a
  union   :: g a -> g a -> g a
  connect :: g a -> g a -> g a

--------------
---RELATION---
--------------

data Relation a = Relation { domain :: Set a, relation :: Set (a, a) }
    deriving (Eq, Show)

connectWith :: (Set a -> [a]) -> Relation a -> Relation a -> Relation a
connectWith f a b = Relation (Set.union (domain a) (domain b)) $ Set.union (Set.union (relation a) (relation b)) (Set.fromList [(x,y) | x <- f (domain a), y <- f (domain b)]) 
 
instance Graph Relation where
    empty = Relation Set.empty Set.empty
    vertex a = Relation (Set.singleton a) Set.empty
    union a b = Relation (Set.union (domain a) (domain b)) (Set.union (relation a) (relation b))
    connect = connectWith Set.elems

instance (Ord a, Num a) => Num (Relation a) where
    fromInteger = vertex . fromInteger
    (+)         = union
    (*)         = connectWith Set.ordElems -- moze jakos szybciej?
    signum      = const empty
    abs         = id
    negate      = id

---------------
-----BASIC-----
---------------

data Basic a = Empty
             | Vertex a
             | Union (Basic a) (Basic a)
             | Connect (Basic a) (Basic a)

fromBasic :: Graph g => Basic a -> g a
fromBasic Empty = empty
fromBasic (Vertex a) = vertex a
fromBasic (Union a b) = fromBasic a `union` fromBasic b
fromBasic (Connect a b) = fromBasic a `connect` fromBasic b

toRelation :: Ord a => Basic a -> Relation a
toRelation = fromBasic

instance Graph Basic where
    empty = Empty
    vertex = Vertex
    union = Union
    connect = Connect

instance Ord a => Eq (Basic a) where
  a == b    =    toRelation a == toRelation b
                      
instance (Ord a, Num a) => Num (Basic a) where
    fromInteger = vertex . fromInteger
    (+)         = union
    (*)         = connect
    signum      = const empty
    abs         = id
    negate      = id

instance Semigroup (Basic a) where
  (<>) = union

instance Monoid (Basic a) where
  mempty = Empty

instance Functor Basic where
    fmap f Empty = Empty
    fmap f (Vertex x) = Vertex (f x)
    fmap f (Union x y) = fmap f x `union` fmap f y
    fmap f (Connect x y) = connect (fmap f x) (fmap f y)  

instance Applicative Basic where
  pure = vertex
  Empty <*> x = Empty
  (Vertex f) <*> x = fmap f x
  (Union f g) <*> x = (f <*> x) `union` (g <*> x)
  (Connect f g) <*> x = (f <*> x) `connect` (g <*> x)

instance Monad Basic where
    return = Vertex
    Empty >>= _ = Empty
    (Vertex x) >>= f = f x
    (Union x y) >>= f = (x >>= f) `union` (y >>= f)
    (Connect x y) >>= f = (x >>= f) `connect` (y >>= f)

ordVertices :: Ord a => Basic a -> [a]
ordVertices = Set.toAscList . domain. toRelation

edges :: Ord a => Basic a -> [(a,a)]
edges = Set.ordElems . relation . toRelation

instance (Ord a, Show a) => Show (Basic a) where
    show a = "edges "  ++ show (edges a) ++ " + vertices " ++ show (ordVertices a)
                                                                               
todot :: (Ord a, Show a) => Basic a -> String
todot a = "digraph {\n" ++ 
          concatMap (\(x,y) -> show x ++ " -> " ++ show y ++ ";\n") (edges a) ++ 
          concatMap (\x -> show x ++ ";\n") (ordVertices a) ++ 
          "}"

mergeV :: Eq a => a -> a -> a -> Basic a -> Basic a
mergeV a b c g = g >>= (\x -> if x == a || x == b then vertex c else vertex x)

splitV :: Eq a => a -> a -> a -> Basic a -> Basic a
splitV a b c g = g >>= (\x -> if x == a then vertex b `union` vertex c else vertex x) 