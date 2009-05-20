module Data.ConsistentHash
    (
      CHash
    , emptyHash
    , isEmpty
    , updateHash
    , findHash
    ) where

import Data.Array.IArray
import Data.List
import Data.Bits
import Data.Int

import Data.ConsistentHash.HashFunc

data CHashItem a = CHashItem Int32 a deriving Show
data CHash a = CHash (a -> Int32) (Array Int32 (CHashItem a))

instance Show (CHash a) where
    show (CHash h a) = "CHash<" ++ (show len) ++ items_name ++ ">"
                        where
                          len = length $ indices a
                          items_name = " item" ++ (if len == 1 then "" else "s")

instance Eq (CHashItem a) where
    (==) (CHashItem m _) (CHashItem n _) = m == n

instance Ord (CHashItem a) where
    compare (CHashItem m _) (CHashItem n _) = compare m n

numTag :: CHashItem a -> Int32
numTag (CHashItem n _) = n

valTag :: CHashItem a -> a
valTag (CHashItem _ v) = v

-- |Create an empty consistent hash from a HashFunc
emptyHash :: HashFunc a -> CHash a
emptyHash hashFunc = CHash hashFunc (array (0, 0) [])

-- |Check whether or not a CHash is empty
isEmpty :: CHash a -> Bool
isEmpty (CHash _ x) = indices x == [0]

addItems :: [CHashItem a] -> CHash a -> CHash a
addItems items (CHash hf ht) = (putStrLn "in addItems") `seq` (CHash hf $ array (0, upBound) newAssocs)
    where
      itemCmp (CHashItem p _) (CHashItem q _) = compare p q
      newElems = sortBy itemCmp $ items ++ extra
      extra     = if indices ht == [0] then [] else elems ht
      newAssocs = zip [0..] newElems
      upBound   = fromIntegral $ (length newElems) - 1

getHashFunc :: CHash a -> (a -> Int32)
getHashFunc (CHash f _) = f

getArray :: CHash a -> Array Int32 (CHashItem a)
getArray (CHash _ arr) = arr

updateHash :: a -> CHash a -> CHash a
updateHash x h = addItems items h
    where
      x' = fromIntegral $ getHashFunc h $ x
      items = [CHashItem p x | p <- cHashInt x']

findHash :: a -> CHash a -> a
findHash needle (CHash func arr) = valTag $ (uncurry binarySearch) (bounds arr)
    where
      targ = func needle
      binarySearch i j
          | i > j     = let ith = arr ! i
                            jth = arr ! j in 
                        if (abs (targ - (numTag ith))) < (abs (targ - (numTag jth)))
                           then ith
                           else jth
          | otherwise = let k = (i + j) `div` 2
                            kth = arr ! k in
                        case compare targ (numTag kth) of
                          LT -> binarySearch i (k-1)
                          EQ -> kth
                          GT -> binarySearch (k+1) j
