module Set(Set(..), empty, null, singleton, union, fromList
              , member, toList, toAscList, elems
          ) where
import Prelude hiding(null)
import Data.List (sort)

data Set a = Empty
           | Singleton a
           | Union (Set a) (Set a)
           deriving(Show)

empty :: Set a
empty = Empty

null :: Set a -> Bool
null Empty = True
null _ = False 

member :: Eq a => a -> Set a -> Bool
member _ Empty              = False
member x (Singleton y)      = x == y
member x (Union y z) = member x y || member x z

singleton :: a -> Set a
singleton = Singleton

fromList :: [a] -> Set a
fromList [] = empty
fromList [x] = singleton x
fromList (x:xs) = singleton x `union` fromList xs

toList :: Set a -> [a]
toList x = go x [] where
    go Empty acc = acc
    go (Singleton x) acc = x:acc  
    go (Union x y) acc = go x (go y acc)

toAscList :: Ord a => Set a -> [a]
toAscList = sort . toList

elems :: Set a -> [a]
elems = toList

union :: Set a -> Set a -> Set a
union = Union

insert :: a -> Set a -> Set a
insert x = union (singleton x)

instance Ord a => Eq (Set a) where
    Singleton a == Singleton b = a == b
    


--instance Semigroup (Set a) where

--instance Monoid (Set a) where

--instance Show a => Show (Set a) where

--instance Functor Set where
