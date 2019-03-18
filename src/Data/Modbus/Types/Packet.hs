module Data.Modbus.Types.Packet
  ( Packet (..)
  , packet
  , decode
  , encode
  , lrcCheck
  ) where

import           Data.Binary                (Binary (..), getWord8, putWord8)
import qualified Data.Binary                as B (decodeOrFail, encode)
import           Data.Bits                  (complement)
import           Data.ByteString.Lazy       (ByteString)
import qualified Data.ByteString.Lazy       as LB (unpack)
import qualified Data.ByteString.Lazy.Char8 as LBC (pack, unpack)
import           Data.Hex                   (hex)
import           Data.Word                  (Word8)

unhexBS :: ByteString -> Maybe ByteString
unhexBS bs = LBC.pack <$> unhex (LBC.unpack bs)

unhex :: String -> Maybe String
unhex []  = Just []
unhex [_] = Nothing
unhex (a:b:r) = do
  x <- c a
  y <- c b
  (toEnum ((x * 16) + y) :) <$> unhex r

c :: Char -> Maybe Int
c '0' = Just 0
c '1' = Just 1
c '2' = Just 2
c '3' = Just 3
c '4' = Just 4
c '5' = Just 5
c '6' = Just 6
c '7' = Just 7
c '8' = Just 8
c '9' = Just 9
c 'A' = Just 10
c 'B' = Just 11
c 'C' = Just 12
c 'D' = Just 13
c 'E' = Just 14
c 'F' = Just 15
c 'a' = Just 10
c 'b' = Just 11
c 'c' = Just 12
c 'd' = Just 13
c 'e' = Just 14
c 'f' = Just 15
c _   = Nothing

data Packet a = Packet
  { packetAddr :: Word8
  , packetData :: a
  , packetLrc  :: Word8
  }
  deriving (Show)

instance Binary a => Binary (Packet a) where
  get = do
    addr <- getWord8
    v <- get
    Packet addr v <$> getWord8

  put (Packet addr a lrc) = do
    putWord8 addr
    put a
    putWord8 lrc

lrcSum :: Binary a => Word8 -> a -> Word8
lrcSum addr a = 1 + complement (fromIntegral $ sum $ map fromEnum (addr : LB.unpack (B.encode a)))

packet :: Binary a => Word8 -> a -> Packet a
packet addr a = Packet addr a (lrcSum addr a)

lrcCheck :: Binary a => Packet a -> Bool
lrcCheck (Packet addr a lrc) = lrcSum addr a == lrc

decode :: Binary a => ByteString -> Maybe (Packet a)
decode bs = f . B.decodeOrFail =<< unhexBS bs
  where f (Right (_, _, p)) = Just p
        f _                 = Nothing

encode :: Binary a => Packet a -> ByteString
encode = hex . B.encode
