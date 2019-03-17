module Data.Modbus.Types.Internal
  ( Address
  , Value
  , NumReg
  , Bit
  , toBitList
  , toWordList
  , toWordList'
  , mkRegOrCoilMap
  ) where

import           Data.Bits (finiteBitSize, setBit, testBit)
import           Data.Word (Word16, Word8)

type Address = Word16
type Value   = Word16
type NumReg  = Word16
type Bit     = Bool

toBitList x = map (testBit x) [0..(finiteBitSize x-1)]

toWordList' :: [Bit] -> [Word8]
toWordList' bits = toWordList bits [] 0 0

toWordList :: [Bit] -> [Word8] -> Word8 -> Int -> [Word8]
toWordList [] w8 i c | c > 0 = w8 ++ [i]
                     | otherwise = w8

toWordList (True:xs) w8 i c | c == 7 = toWordList xs (w8 ++ [setBit i c]) 0 0
                            | otherwise = toWordList xs w8 (setBit i c) (c + 1)

toWordList (False:xs) w8 i c | c == 7 = toWordList xs (w8 ++ [i]) 0 0
                             | otherwise = toWordList xs w8 i (c + 1)

mkRegOrCoilMap :: Address -> [a] -> [(Address, a)]
mkRegOrCoilMap _ []     = []
mkRegOrCoilMap s (x:xs) = (s, x) : mkRegOrCoilMap (s + 1) xs
