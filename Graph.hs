module Graph where
import Set(Set)
import qualified Set as Set
import Data.List (sort)
class Graph g where
  empty   :: g a
  vertex  :: a -> g a
  union   :: g a -> g a -> g a
  connect :: g a -> g a -> g a

-- RELATION

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

-- BASIC

data Basic a = Empty
             | Vertex a
             | Union (Basic a) (Basic a)
             | Connect (Basic a) (Basic a)

basicDomain :: Basic a -> [a]
basicDomain (Vertex a) = [a]
basicDomain Empty = []
basicDomain (Union a b) = basicDomain a ++ basicDomain b
basicDomain (Connect a b) = basicDomain a ++ basicDomain b
{-
basicVertices :: Basic a -> [a]
basicVertices (Connect a b) = []
basicVertices = basicDomain
-}
basicRelation :: Basic a -> [(a, a)]
basicRelation Empty = []
basicRelation (Vertex a) = []
basicRelation (Union a b) = basicRelation a ++ basicRelation b
basicRelation (Connect a b) = basicRelation a ++ basicRelation b ++ [(x,y) | x <- basicDomain a, y <- basicDomain b]

instance Graph Basic where
    empty = Empty
    vertex = Vertex
    union = Union
    
    connect Empty a = a
    connect a Empty = a
    connect a b = Connect a b

instance Ord a => Eq (Basic a) where
  a == b    =  (verticesOrd a) == (verticesOrd b) && (edges a) == (edges b)

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

fromBasic :: Graph g => Basic a -> g a
fromBasic Empty = empty
fromBasic (Vertex a) = vertex a
fromBasic (Union a b) = fromBasic a `union` fromBasic b
fromBasic (Connect a b) = fromBasic a `connect` fromBasic b

vertices :: Basic a -> [a]
vertices Empty = []
vertices (Connect _ _) = []
vertices (Vertex a) = [a]
vertices (Union a b) = vertices a ++ vertices b

verticesOrd :: Ord a => Basic a -> [a]
verticesOrd = Set.nubOrdered . sort . vertices

edges :: Ord a => Basic a -> [(a,a)]
edges = Set.nubOrdered . sort . basicRelation

instance (Ord a, Show a) => Show (Basic a) where
    show a = "edges " ++ show (edges a) ++ " + vertices " ++ show (verticesOrd a)
      
todot :: (Ord a, Show a) => Basic a -> String
todot a = "digraph {\n" ++ 
          (concat (map (\(x,y) -> show x ++ " -> " ++ show y ++ ";\n") (edges a))) ++ 
          (concat (map (\x -> show x ++ ";\n") (vertices a))) ++ 
          "}"

mergeV :: Eq a => a -> a -> a -> Basic a -> Basic a
mergeV a b c g = undefined

{-
-- | Example graph
-- >>> example34
-- edges [(1,2),(2,3),(2,4),(3,5),(4,5)] + vertices [17]

example34 :: Basic Int
example34 = 1*2 + 2*(3+4) + (3+4)*5 + 17

instance Functor Basic where

-- | Merge vertices
-- >>> mergeV 3 4 34 example34
-- edges [(1,2),(2,34),(34,5)] + vertices [17]

instance Applicative Basic where

instance Monad Basic where

-- | Split Vertex
-- >>> splitV 34 3 4 (mergeV 3 4 34 example34)
-- edges [(1,2),(2,3),(2,4),(3,5),(4,5)] + vertices [17]

splitV :: Eq a => a -> a -> a -> Basic a -> Basic a
splitV = undefined

-}