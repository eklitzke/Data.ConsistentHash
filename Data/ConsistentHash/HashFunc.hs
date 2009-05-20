module Data.ConsistentHash.HashFunc
    ( 
      HashFunc
    , hashInt
    , cHashInt
    ) where

import Codec.Utils
import Data.Bits
import Data.Digest.MD5
import Data.Int
import Data.List

-- |The type of a hash function.
type HashFunc a = a -> Int32

-- |A good hash function for Ints.
hashInt :: HashFunc Int
hashInt = head . md5Hash

-- |Covert a list of octets to an Int32. Caller should ensure the list passed in
-- has length <= 4.
octetsToInt32 :: [Octet] -> Int32
octetsToInt32 []  = 0
octetsToInt32 os = octets' 0 os
    where
      octets' :: Int32 -> [Octet] -> Int32
      octets' n [] = n
      octets' n (x:xs) = octets' ((n `shiftL` 8) .|. (fromIntegral x)) xs

-- |Hash an Int, and convert the result to a list of exactly four Int32s. This
-- is useful for the main Data.ConsistentHash module, which needs to hash a 
-- single value into many different highly variable hashes.
md5Hash :: Int -> [Int32]
md5Hash = groupOctets . hash . safeToOctets
    where
      safeToOctets x = toOctets (if x < 0 then -256 else 256) x
      groupOctets [] = []
      groupOctets xs = octetsToInt32 xsFront : groupOctets xsBack
          where xsFront = take 4 xs
                xsBack  = drop 4 xs

-- |Hash an Int to exactly length 256 list of [Int32]
cHashInt :: Int -> [Int32]
cHashInt = sort . concatMap (md5Hash' . fromIntegral) . md5Hash'
    where md5Hash' = concatMap (md5Hash . fromIntegral) . md5Hash
