{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Data.Modbus.AddrMap
  ( Code (..)
  , AddrMap
  , lookupAddr
  , lookupCode
  , pairCode
  ) where

import           Data.Aeson.Types   (Pair, (.=))
import           Data.Binary        (Binary (..), getWord8, putWord8)
import           Data.Binary.Get    (getByteString, getWord16be)
import           Data.Binary.Put    (putByteString, putWord16be)
import qualified Data.ByteString    as B (length)
import           Data.List          (find)
import           Data.Text          (Text)
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)
import           Data.Word          (Word16)

data Code = Code
  { addr    :: Word16
  , name    :: Text
  , comment :: Maybe Text
  }

instance Binary Code where
  get = do
    addr <- getWord16be
    len <- fromIntegral <$> getWord8
    name <- decodeUtf8 <$> getByteString len

    len1 <- fromIntegral <$> getWord8
    comment <- if len1 > 0 then
      Just . decodeUtf8 <$> getByteString len1
    else pure Nothing

    pure Code {..}

  put Code {..} = do
    putWord16be addr
    let bsN = encodeUtf8 name
    putWord8 $ fromIntegral $ B.length bsN
    putByteString bsN
    case comment of
      Nothing -> putWord8 0
      Just c -> do
        let bsC = encodeUtf8 c
        putWord8 $ fromIntegral $ B.length bsC
        putByteString bsC


type AddrMap = [Code]

lookupAddr :: Text -> AddrMap -> Maybe Word16
lookupAddr n m = addr <$> find (\x -> name x == n) m

lookupCode :: Word16 -> AddrMap -> Maybe Code
lookupCode n = find (\x -> addr x == n)

pairCode :: AddrMap -> Word16 -> [Pair]
pairCode m k =
  case lookupCode k m of
    Nothing        -> []
    Just Code {..} -> [ "comment" .= comment, "name" .= name ]
