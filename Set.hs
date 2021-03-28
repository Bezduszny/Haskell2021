module Set(Set(..), empty, null, singleton, union, fromList
              , member, toList, toAscList, elems, ordElems, nubOrdered
          ) where
import Prelude hiding(null)
import Data.List (sort)

data Set a = Empty
           | Singleton a
           | Union (Set a) (Set a)

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
toAscList = nubOrdered . sort . toList

nubOrdered :: Ord a => [a] -> [a]
nubOrdered [] = []
nubOrdered [x] = [x]
nubOrdered (x:xs) = x:go x xs where
    go _ [] = []
    go x (y:ys)     |   x /= y    = y : go y ys
                    |   x == y    = go x ys

elems :: Set a -> [a]
elems = toList

ordElems :: Ord a => Set a -> [a]
ordElems = nubOrdered . toAscList

union :: Set a -> Set a -> Set a
union Empty a = a
union a Empty = a
union a b = Union a b

insert :: a -> Set a -> Set a
insert x = union (singleton x)

instance Ord a => Eq (Set a) where
    Singleton a == Singleton b          =       a == b
    Empty == Empty                      =       True
    Empty == _                          =       False 
    _ == Empty                          =       False
    a == b                              =       nubOrdered (toAscList a) == nubOrdered (toAscList b)


instance Semigroup (Set a) where
  Empty <> a = a
  a <> Empty = a
  a <> b = a `union` b

instance Monoid (Set a) where
    mempty = Empty
    mappend = union

instance Show a => Show (Set a) where
   showsPrec d Empty = showParen (d > 0) $ showString "Empty"
   showsPrec d (Singleton a) = showParen (d > 0) $ showString $ "Singleton " ++ show a
   showsPrec d (Union a b) = showParen (d > 0) $
        showString "Union " .
        showsPrec 1 a       .
        showString " "      .
        showsPrec 1 b

instance Functor Set where
    fmap f Empty = Empty
    fmap f (Singleton a) = singleton $ f a
    fmap f (Union a b) = fmap f a `union` fmap f b
