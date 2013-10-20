{-# LANGUAGE Rank2Types, FlexibleContexts #-}

module Data.Existential.Map.Lazy (
    -- * Map type
      Map

    -- * Operators
    , (!), (\\)

    -- * Query
    , null
    , size
    , member
    , notMember
    , lookup
    , findWithDefault
    , lookupLT
    , lookupGT
    , lookupLE
    , lookupGE

    -- * Construction
    , empty
    , singleton

    -- ** Insertion
    , insert
    , insertWith
    , insertWithKey
    , insertLookupWithKey

    -- ** Delete\/Update
    , delete
    , adjust
    , adjustWithKey
    , update
    , updateWithKey
    , updateLookupWithKey
    , alter

    -- * Combine

    -- ** Union
    , union
    , unionWith
    , unionWithKey
    , unions
    , unionsWith

    -- ** Difference
    , difference
    , differenceWith
    , differenceWithKey

    -- ** Intersection
    , intersection
    , intersectionWith
    , intersectionWithKey

    -- ** Universal combining function
    , mergeWithKey

    -- * Traversal
    -- ** Map
    , map
    , mapWithKey
    , traverseWithKey
    , mapAccum
    , mapAccumWithKey
    , mapAccumRWithKey
    , mapKeys
    , mapKeysWith
    , mapKeysMonotonic

    -- * Folds
    , foldr
    , foldl
    , foldrWithKey
    , foldlWithKey
    , foldMapWithKey

    -- ** Strict folds
    , foldr'
    , foldl'
    , foldrWithKey'
    , foldlWithKey'

    -- * Conversion
    , elems
    , keys
    , assocs
    , keysSet
    , fromSet

    -- ** Lists
    , toList
    , fromList
    , fromListWith
    , fromListWithKey

    -- ** Ordered lists
    , toAscList
    , toDescList
    , fromAscList
    , fromAscListWith
    , fromAscListWithKey
    , fromDistinctAscList

    -- * Filter
    , filter
    , filterWithKey
    , partition
    , partitionWithKey

    , mapMaybe
    , mapMaybeWithKey
    , mapEither
    , mapEitherWithKey

    , split
    , splitLookup

    -- * Submap
    , isSubmapOf, isSubmapOfBy
    , isProperSubmapOf, isProperSubmapOfBy

    -- * Indexed
    , lookupIndex
    , findIndex
    , elemAt
    , updateAt
    , deleteAt

    -- * Min\/Max
    , findMin
    , findMax
    , deleteMin
    , deleteMax
    , deleteFindMin
    , deleteFindMax
    , updateMin
    , updateMax
    , updateMinWithKey
    , updateMaxWithKey
    , minView
    , maxView
    , minViewWithKey
    , maxViewWithKey

    -- * Debugging
    , showTree
    , showTreeWith
    , valid
    ) where

import qualified Data.Map.Lazy as M
import qualified Data.Set as S
import Data.Monoid (Monoid)
import Control.Applicative (Applicative)
import Data.Existential
import Data.Functor.Product
import Unsafe.Coerce

import Prelude hiding (null, lookup, map, foldr, foldl, filter)

newtype Map k v = Map {unMap :: M.Map (Some k) (Some v)}

(!) :: Ord (Some k) => Map k a -> k i -> a i
Map m ! k = unsafeGet (m M.! Some k)

(\\) :: Ord (Some k) => Map k a -> Map k b -> Map k a
Map m1 \\ Map m2 = Map (m1 M.\\ m2)

---------------------

null :: Map k a -> Bool
null (Map m) = M.null m

size :: Map k a -> Int
size (Map m) = M.size m

member :: Ord (Some k) => k i -> Map k a -> Bool
member k (Map m) = M.member (Some k) m

notMember :: Ord (Some k) => k i -> Map k a -> Bool
notMember k (Map m) = M.notMember (Some k) m

lookup :: Ord (Some k) => k i -> Map k a -> Maybe (a i)
lookup k (Map m) = fmap unsafeGet (M.lookup (Some k) m)

findWithDefault :: Ord (Some k) => a i -> k i -> Map k a -> a i
findWithDefault def k (Map m) = unsafeGet (M.findWithDefault (Some def) (Some k) m)

lookupLT :: Ord (Some k) => k i -> Map k v -> Maybe (Some (Product k v))
lookupLT k (Map m) = fmap unsafeProd (M.lookupLT (Some k) m)

lookupGT :: Ord (Some k) => k i -> Map k v -> Maybe (Some (Product k v))
lookupGT k (Map m) = fmap unsafeProd (M.lookupGT (Some k) m)

lookupLE :: Ord (Some k) => k i -> Map k v -> Maybe (Some (Product k v))
lookupLE k (Map m) = fmap unsafeProd (M.lookupLE (Some k) m)

lookupGE :: Ord (Some k) => k i -> Map k v -> Maybe (Some (Product k v))
lookupGE k (Map m) = fmap unsafeProd (M.lookupGE (Some k) m)

-----------------------------------

empty :: Map k a
empty = Map M.empty

singleton :: k i -> a i -> Map k a
singleton k a = Map (M.singleton (Some k) (Some a))

insert :: Ord (Some k) => k i -> a i -> Map k a -> Map k a
insert k a (Map m) = Map (M.insert (Some k) (Some a) m)

insertWith :: Ord (Some k) => (a i -> a i -> a i) -> k i -> a i -> Map k a -> Map k a
insertWith f k a (Map m) = Map (M.insertWith f' (Some k) (Some a) m) where
    f' x y = Some (f (unsafeGet x) (unsafeGet y))

insertWithKey :: Ord (Some k) => (k i -> a i -> a i -> a i) -> k i -> a i -> Map k a -> Map k a
insertWithKey f k a (Map m) = Map (M.insertWithKey f' (Some k) (Some a) m) where
    f' k' x y = Some (f (unsafeGet k') (unsafeGet x) (unsafeGet y))

insertLookupWithKey :: Ord (Some k) => (k i -> a i -> a i -> a i) -> k i -> a i -> Map k a -> (Maybe (a i), Map k a)
insertLookupWithKey f k a (Map m) = (fmap unsafeGet a', Map m') where
    (a', m') = M.insertLookupWithKey f' (Some k) (Some a) m
    f' k' x y = Some (f (unsafeGet k') (unsafeGet x) (unsafeGet y))

-------------------------------

delete :: Ord (Some k) => k i -> Map k a -> Map k a
delete k (Map m) = Map (M.delete (Some k) m)

adjust :: Ord (Some k) => (a i -> a i) -> k i -> Map k a -> Map k a
adjust f k (Map m) = Map (M.adjust f' (Some k) m) where
    f' a = Some (f (unsafeGet a))

adjustWithKey :: Ord (Some k) => (k i -> a i -> a i) -> k i -> Map k a -> Map k a
adjustWithKey f k (Map m) = Map (M.adjustWithKey f' (Some k) m) where
    f' k' a = Some (f (unsafeGet k') (unsafeGet a))

update :: Ord (Some k) => (a i -> Maybe (a i)) -> k i -> Map k a -> Map k a
update f k (Map m) = Map (M.update f' (Some k) m) where
    f' a = fmap Some (f (unsafeGet a))

updateWithKey :: Ord (Some k) => (k i -> a i -> Maybe (a i)) -> k i -> Map k a -> Map k a
updateWithKey f k (Map m) = Map (M.updateWithKey f' (Some k) m) where
    f' k' a = fmap Some (f (unsafeGet k') (unsafeGet a))

updateLookupWithKey :: Ord (Some k) => (k i -> a i -> Maybe (a i)) -> k i -> Map k a -> (Maybe (a i), Map k a)
updateLookupWithKey f k (Map m) = (fmap unsafeGet a', Map m') where
    (a', m') = M.updateLookupWithKey f' (Some k) m
    f' k' a = fmap Some (f (unsafeGet k') (unsafeGet a))

alter :: Ord (Some k) => (Maybe (a i) -> Maybe (a i)) -> k i -> Map k a -> Map k a
alter f k (Map m) = Map (M.alter f' (Some k) m) where
    f' a = fmap Some (f (fmap unsafeGet a))

union :: Ord (Some k) => Map k a -> Map k a -> Map k a
union (Map m1) (Map m2) = Map (M.union m1 m2)

unionWith :: Ord (Some k) => (forall i . a i -> a i -> a i) -> Map k a -> Map k a -> Map k a
unionWith f (Map m1) (Map m2) = Map (M.unionWith f' m1 m2) where
    f' a b = Some (f (unsafeGet a) (unsafeGet b))

unionWithKey :: Ord (Some k) => (forall i . k i -> a i -> a i -> a i) -> Map k a -> Map k a -> Map k a
unionWithKey f (Map m1) (Map m2) = Map (M.unionWithKey f' m1 m2) where
    f' k a b = Some (f (unsafeGet k) (unsafeGet a) (unsafeGet b))

-- TODO: Use the new cheap newtype conversions instead of map.
unions :: Ord (Some k) => [Map k a] -> Map k a
unions ms = Map (M.unions (fmap unMap ms))

unionsWith :: Ord (Some k) => (forall i . a i -> a i -> a i) -> [Map k a] -> Map k a
unionsWith f ms = Map (M.unionsWith f' (fmap unMap ms)) where
    f' a b = Some (f (unsafeGet a) (unsafeGet b))

----------------------------------

difference :: Ord (Some k) => Map k a -> Map k b -> Map k a
difference (Map m1) (Map m2) = Map (M.difference m1 m2)

differenceWith :: Ord (Some k) => (forall i . a i -> b i -> Maybe (a i)) -> Map k a -> Map k b -> Map k a
differenceWith f (Map m1) (Map m2) = Map (M.differenceWith f' m1 m2) where
    f' a b = fmap Some (f (unsafeGet a) (unsafeGet b))

differenceWithKey :: Ord (Some k) => (forall i . k i -> a i -> b i -> Maybe (a i)) -> Map k a -> Map k b -> Map k a
differenceWithKey f (Map m1) (Map m2) = Map (M.differenceWithKey f' m1 m2) where
    f' k a b = fmap Some (f (unsafeGet k) (unsafeGet a) (unsafeGet b))

----------------------------------

intersection :: Ord (Some k) => Map k a -> Map k b -> Map k a
intersection (Map m1) (Map m2) = Map (M.intersection m1 m2)

intersectionWith :: Ord (Some k) => (forall i . a i -> b i -> c i) -> Map k a -> Map k b -> Map k c
intersectionWith f (Map m1) (Map m2) = Map (M.intersectionWith f' m1 m2) where
    f' a b = Some (f (unsafeGet a) (unsafeGet b))

intersectionWithKey :: Ord (Some k) => (forall i . k i -> a i -> b i -> c i) -> Map k a -> Map k b -> Map k c
intersectionWithKey f (Map m1) (Map m2) = Map (M.intersectionWithKey f' m1 m2) where
    f' k a b = Some (f (unsafeGet k) (unsafeGet a) (unsafeGet b))

----------------------------------

mergeWithKey :: Ord (Some k) => (forall i . k i -> a i -> b i -> Maybe (c i)) -> (Map k a -> Map k c) -> (Map k b -> Map k c) -> Map k a -> Map k b -> Map k c
mergeWithKey combine only1 only2 (Map m1) (Map m2) = Map (M.mergeWithKey combine' only1' only2' m1 m2) where
    combine' k a b = fmap Some (combine (unsafeGet k) (unsafeGet a) (unsafeGet b))
    only1' m = unMap (only1 (Map m))
    only2' m = unMap (only2 (Map m))

------------------------------------

map :: (forall i . a i -> b i) -> Map k a -> Map k b
map f (Map m) = Map (M.map f' m) where
    f' a = Some (f (unsafeGet a))

mapWithKey :: (forall i . k i -> a i -> b i) -> Map k a -> Map k b
mapWithKey f (Map m) = Map (M.mapWithKey f' m) where
    f' k a = Some (f (unsafeGet k) (unsafeGet a))

traverseWithKey :: Applicative t => (forall i . k i -> a i -> t (b i)) -> Map k a -> t (Map k b)
traverseWithKey f (Map m) = fmap Map (M.traverseWithKey f' m) where
    f' k a = fmap Some (f (unsafeGet k) (unsafeGet a))

mapAccum :: (forall i . a -> b i -> (a, c i)) -> a -> Map k b -> (a, Map k c)
mapAccum f a (Map m) = (a', Map m') where
    (a', m') = M.mapAccum f' a m
    f' a b = fmap Some (f a (unsafeGet b))

mapAccumWithKey :: (forall i . a -> k i -> b i -> (a, c i)) -> a -> Map k b -> (a, Map k c)
mapAccumWithKey f a (Map m) = (a', Map m') where
    (a', m') = M.mapAccumWithKey f' a m
    f' a k b = fmap Some (f a (unsafeGet k) (unsafeGet b))

mapAccumRWithKey :: (forall i . a -> k i -> b i -> (a, c i)) -> a -> Map k b -> (a, Map k c)
mapAccumRWithKey f a (Map m) = (a', Map m') where
    (a', m') = M.mapAccumRWithKey f' a m
    f' a k b = fmap Some (f a (unsafeGet k) (unsafeGet b))

mapKeys :: Ord (Some k2) => (forall i . k1 i -> k2 i) -> Map k1 a -> Map k2 a
mapKeys f (Map m) = Map (M.mapKeys f' m) where
    f' k = Some (f (unsafeGet k))

mapKeysWith :: Ord (Some k2) => (forall i . a i -> a i -> a i) -> (forall i . k1 i -> k2 i) -> Map k1 a -> Map k2 a
mapKeysWith combine f (Map m) = Map (M.mapKeysWith combine' f' m) where
    f' k = Some (f (unsafeGet k))
    combine' a b = Some (combine (unsafeGet a) (unsafeGet b))

mapKeysMonotonic :: Ord (Some k2) => (forall i . k1 i -> k2 i) -> Map k1 a -> Map k2 a
mapKeysMonotonic f (Map m) = Map (M.mapKeysMonotonic f' m) where
    f' k = Some (f (unsafeGet k))

---------------------------------

foldr :: (forall i . a i -> b -> b) -> b -> Map k a -> b
foldr f z (Map m) = M.foldr f' z m where
    f' a b = f (unsafeGet a) b

foldl :: (forall i . a -> b i -> a) -> a -> Map k b -> a
foldl f z (Map m) = M.foldl f' z m where
    f' a b = f a (unsafeGet b)

foldrWithKey :: (forall i . k i -> a i -> b -> b) -> b -> Map k a -> b
foldrWithKey f z (Map m) = M.foldrWithKey f' z m where
    f' k a b = f (unsafeGet k) (unsafeGet a) b

foldlWithKey :: (forall i . a -> k i -> b i -> a) -> a -> Map k b -> a
foldlWithKey f z (Map m) = M.foldlWithKey f' z m where
    f' a k b = f a (unsafeGet k) (unsafeGet b)

foldMapWithKey :: Monoid m => (forall i . k i -> a i -> m) -> Map k a -> m
foldMapWithKey f (Map m) = M.foldMapWithKey f' m where
    f' k a = f (unsafeGet k) (unsafeGet a)

------------------------------------

foldr' :: (forall i . a i -> b -> b) -> b -> Map k a -> b
foldr' f z (Map m) = M.foldr' f' z m where
    f' a b = f (unsafeGet a) b

foldl' :: (forall i . a -> b i -> a) -> a -> Map k b -> a
foldl' f z (Map m) = M.foldl' f' z m where
    f' a b = f a (unsafeGet b)

foldrWithKey' :: (forall i . k i -> a i -> b -> b) -> b -> Map k a -> b
foldrWithKey' f z (Map m) = M.foldrWithKey' f' z m where
    f' k a b = f (unsafeGet k) (unsafeGet a) b

foldlWithKey' :: (forall i . a -> k i -> b i -> a) -> a -> Map k b -> a
foldlWithKey' f z (Map m) = M.foldlWithKey' f' z m where
    f' a k b = f a (unsafeGet k) (unsafeGet b)

--------------------------------------

elems :: Map k a -> [Some a]
elems (Map m) = M.elems m

keys :: Map k a -> [Some k]
keys (Map m) = M.keys m

assocs :: Map k a -> [Some (Product k a)]
assocs (Map m) = fmap unsafeProd (M.assocs m)

keysSet :: Map k a -> S.Set (Some k)
keysSet (Map m) = M.keysSet m

fromSet :: (forall i . k i -> a i) -> S.Set (Some k) -> Map k a
fromSet f s = Map (M.fromSet f' s) where
    f' k = Some (f (unsafeGet k))

----------------------------------------

toList :: Map k a -> [Some (Product k a)]
toList (Map m) = fmap unsafeProd (M.toList m)

fromList :: Ord (Some k) => [Some (Product k a)] -> Map k a
fromList l = Map (M.fromList (fmap fromProd l))

fromListWith :: Ord (Some k) => (forall i . a i -> a i -> a i) -> [Some (Product k a)] -> Map k a
fromListWith f l = Map (M.fromListWith f' (fmap fromProd l)) where
    f' a b = Some (f (unsafeGet a) (unsafeGet b))

fromListWithKey :: Ord (Some k) => (forall i . k i -> a i -> a i -> a i) -> [Some (Product k a)] -> Map k a
fromListWithKey f l = Map (M.fromListWithKey f' (fmap fromProd l)) where
    f' k a b = Some (f (unsafeGet k) (unsafeGet a) (unsafeGet b))

-------------------------------

toAscList :: Map k a -> [Some (Product k a)]
toAscList (Map m) = fmap unsafeProd (M.toAscList m)

toDescList :: Map k a -> [Some (Product k a)]
toDescList (Map m) = fmap unsafeProd (M.toDescList m)

fromAscList :: Eq (Some k) => [Some (Product k a)] -> Map k a
fromAscList l = Map (M.fromAscList (fmap fromProd l))

fromAscListWith :: Eq (Some k) => (forall i . a i -> a i -> a i) -> [Some (Product k a)] -> Map k a
fromAscListWith f l = Map (M.fromAscListWith f' (fmap fromProd l)) where
    f' a b = Some (f (unsafeGet a) (unsafeGet b))

fromAscListWithKey :: Eq (Some k) => (forall i . k i -> a i -> a i -> a i) -> [Some (Product k a)] -> Map k a
fromAscListWithKey f l = Map (M.fromAscListWithKey f' (fmap fromProd l)) where
    f' k a b = Some (f (unsafeGet k) (unsafeGet a) (unsafeGet b))

fromDistinctAscList :: [Some (Product k a)] -> Map k a
fromDistinctAscList l = Map (M.fromDistinctAscList (fmap fromProd l))

----------------------------------

filter :: (forall i . a i -> Bool) -> Map k a -> Map k a
filter p (Map m) = Map (M.filter p' m) where
    p' a = p (unsafeGet a)

filterWithKey :: (forall i . k i -> a i -> Bool) -> Map k a -> Map k a
filterWithKey p (Map m) = Map (M.filterWithKey p' m) where
    p' k a = p (unsafeGet k) (unsafeGet a)

partition :: (forall i . a i -> Bool) -> Map k a -> (Map k a, Map k a)
partition p (Map m) = (Map m1, Map m2) where
    (m1, m2) = M.partition p' m
    p' a = p (unsafeGet a)

partitionWithKey :: (forall i . k i -> a i -> Bool) -> Map k a -> (Map k a, Map k a)
partitionWithKey p (Map m) = (Map m1, Map m2) where
    (m1, m2) = M.partitionWithKey p' m
    p' k a = p (unsafeGet k) (unsafeGet a)

mapMaybe :: (forall i . a i -> Maybe (b i)) -> Map k a -> Map k b
mapMaybe f (Map m) = Map (M.mapMaybe f' m) where 
    f' a = fmap Some (f (unsafeGet a))

mapMaybeWithKey :: (forall i . k i -> a i -> Maybe (b i)) -> Map k a -> Map k b
mapMaybeWithKey f (Map m) = Map (M.mapMaybeWithKey f' m) where 
    f' k a = fmap Some (f (unsafeGet k) (unsafeGet a))

mapEither :: (forall i . a i -> Either (b i) (c i)) -> Map k a -> (Map k b, Map k c)
mapEither f (Map m) = (Map m1, Map m2) where
    (m1, m2) = M.mapEither f' m
    f' a = case f (unsafeGet a) of
        Left b -> Left (Some b)
        Right c -> Right (Some c)

mapEitherWithKey :: (forall i . k i -> a i -> Either (b i) (c i)) -> Map k a -> (Map k b, Map k c)
mapEitherWithKey f (Map m) = (Map m1, Map m2) where
    (m1, m2) = M.mapEitherWithKey f' m
    f' k a = case f (unsafeGet k) (unsafeGet a) of
        Left b -> Left (Some b)
        Right c -> Right (Some c)

split :: Ord (Some k) => k i -> Map k a -> (Map k a, Map k a)
split k (Map m) = (Map m1, Map m2) where
    (m1, m2) = M.split (Some k) m

splitLookup :: Ord (Some k) => k i -> Map k a -> (Map k a, Maybe (a i), Map k a)
splitLookup k (Map m) = (Map m1, fmap unsafeGet a, Map m2) where
    (m1, a, m2) = M.splitLookup (Some k) m

--------------------------

isSubmapOf :: (Ord (Some k), Eq (Some a)) => Map k a -> Map k a -> Bool
Map m1 `isSubmapOf` Map m2 = m1 `M.isSubmapOf` m2

isSubmapOfBy :: Ord (Some k) => (forall i . a i -> b i -> Bool) -> Map k a -> Map k b -> Bool
isSubmapOfBy f (Map m1) (Map m2) = M.isSubmapOfBy f' m1 m2 where
    f' a b = f (unsafeGet a) (unsafeGet b)

isProperSubmapOf :: (Ord (Some k), Eq (Some a)) => Map k a -> Map k a -> Bool
Map m1 `isProperSubmapOf` Map m2 = m1 `M.isProperSubmapOf` m2

isProperSubmapOfBy :: Ord (Some k) => (forall i . a i -> b i -> Bool) -> Map k a -> Map k b -> Bool
isProperSubmapOfBy f (Map m1) (Map m2) = M.isProperSubmapOfBy f' m1 m2 where
    f' a b = f (unsafeGet a) (unsafeGet b)

------------------------------------

lookupIndex :: Ord (Some k) => k i -> Map k a -> Maybe Int
lookupIndex k (Map m) = M.lookupIndex (Some k) m

findIndex :: Ord (Some k) => k i -> Map k a -> Int
findIndex k (Map m) = M.findIndex (Some k) m

elemAt :: Int -> Map k a -> Some (Product k a)
elemAt i (Map m) = unsafeProd (M.elemAt i m)

updateAt :: (forall i . k i -> a i -> Maybe (a i)) -> Int -> Map k a -> Map k a
updateAt f i (Map m) = Map (M.updateAt f' i m) where
    f' k a = fmap Some (f (unsafeGet k) (unsafeGet a))

deleteAt :: Int -> Map k a -> Map k a
deleteAt i (Map m) = Map (M.deleteAt i m)

----------------------------------------

findMin :: Map k a -> Some (Product k a)
findMin (Map m) = unsafeProd (M.findMin m)

findMax :: Map k a -> Some (Product k a)
findMax (Map m) = unsafeProd (M.findMax m)

deleteMin :: Map k a -> Map k a
deleteMin (Map m) = Map (M.deleteMin m)

deleteMax :: Map k a -> Map k a
deleteMax (Map m) = Map (M.deleteMax m)

deleteFindMin :: Map k a -> (Some (Product k a), Map k a)
deleteFindMin (Map m) = (unsafeProd ka, Map m') where
    (ka, m') = M.deleteFindMin m

deleteFindMax :: Map k a -> (Some (Product k a), Map k a)
deleteFindMax (Map m) = (unsafeProd ka, Map m') where
    (ka, m') = M.deleteFindMax m

updateMin :: (forall i . a i -> Maybe (a i)) -> Map k a -> Map k a
updateMin f (Map m) = Map (M.updateMin f' m) where
    f' a = fmap Some (f (unsafeGet a))

updateMax :: (forall i . a i -> Maybe (a i)) -> Map k a -> Map k a
updateMax f (Map m) = Map (M.updateMax f' m) where
    f' a = fmap Some (f (unsafeGet a))

updateMinWithKey :: (forall i . k i -> a i -> Maybe (a i)) -> Map k a -> Map k a
updateMinWithKey f (Map m) = Map (M.updateMinWithKey f' m) where
    f' k a = fmap Some (f (unsafeGet k) (unsafeGet a))

updateMaxWithKey :: (forall i . k i -> a i -> Maybe (a i)) -> Map k a -> Map k a
updateMaxWithKey f (Map m) = Map (M.updateMaxWithKey f' m) where
    f' k a = fmap Some (f (unsafeGet k) (unsafeGet a))

minView :: Map k a -> Maybe (Some a, Map k a)
minView (Map m) = fmap change (M.minView m) where
    change (a, m') = (a, Map m')

maxView :: Map k a -> Maybe (Some a, Map k a)
maxView (Map m) = fmap change (M.maxView m) where
    change (a, m') = (a, Map m')

minViewWithKey :: Map k a -> Maybe (Some (Product k a), Map k a)
minViewWithKey (Map m) = fmap change (M.minViewWithKey m) where
    change (ka, m') = (unsafeProd ka, Map m')

maxViewWithKey :: Map k a -> Maybe (Some (Product k a), Map k a)
maxViewWithKey (Map m) = fmap change (M.maxViewWithKey m) where
    change (ka, m') = (unsafeProd ka, Map m')

-----------------------------------

showTree :: (Show (Some k), Show (Some a)) => Map k a -> String
showTree (Map m) = M.showTree m

showTreeWith :: (forall i . k i -> a i -> String) -> Bool -> Bool -> Map k a -> String
showTreeWith showelem hang wide (Map m) = M.showTreeWith showelem' hang wide m where
    showelem' k a = showelem (unsafeGet k) (unsafeGet a)

valid :: Ord (Some k) => Map k a -> Bool
valid (Map m) = M.valid m

---------- Internal ----------

unsafeGet :: Some k -> k i
unsafeGet (Some k) = unsafeCoerce k

unsafeProd :: (Some k, Some a) -> Some (Product k a)
unsafeProd (k, a) = Some (Pair (unsafeGet k) (unsafeGet a))

fromProd :: Some (Product k a) -> (Some k, Some a)
fromProd (Some (Pair k a)) = (Some k, Some a)
