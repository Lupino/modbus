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
import           Data.Hex                   (hex, unhex)
import           Data.Word                  (Word8)

data Packet a = Packet
  { packetAddr :: !Word8
  , packetData :: !a
  , packetLrc  :: !Word8
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
decode bs = f . B.decodeOrFail =<< unhex bs
  where f (Right (_, _, p)) = Just p
        f _                 = Nothing

encode :: Binary a => Packet a -> ByteString
encode = hex . B.encode
