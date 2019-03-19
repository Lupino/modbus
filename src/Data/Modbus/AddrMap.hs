{-# LANGUAGE RecordWildCards #-}

module Data.Modbus.AddrMap
  ( Code (..)
  , AddrMap
  , lookupAddr
  , lookupCode
  ) where

import           Data.Binary        (Binary (..), getWord8, putWord8)
import           Data.Binary.Get    (getByteString, getWord16be)
import           Data.Binary.Put    (putByteString, putWord16be)
import           Data.List          (find)
import           Data.Text          (Text)
import qualified Data.Text          as T (length)
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
    putWord8 $ fromIntegral $ T.length name
    putByteString $ encodeUtf8 name
    case comment of
      Nothing -> putWord8 0
      Just c -> do
        putWord8 $ fromIntegral $ T.length c
        putByteString $ encodeUtf8 c


type AddrMap = [Code]

lookupAddr :: Text -> AddrMap -> Maybe Word16
lookupAddr n m = addr <$> find (\x -> name x == n) m

lookupCode :: Word16 -> AddrMap -> Maybe Code
lookupCode n = find (\x -> addr x == n)
