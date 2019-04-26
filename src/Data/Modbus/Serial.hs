{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Modbus.Serial
  ( Serial (..)
  , recvPacket
  , sendPacket
  ) where

import           Control.Concurrent.STM (TVar, atomically, readTVar, writeTVar)
import           Control.Monad          (when)
import           Data.Binary            (Binary (..))
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as B (drop, dropWhile, elem, head,
                                              length, takeWhile)
import           Data.ByteString.Lazy   (fromStrict, toStrict)
import qualified Data.ByteString.Lazy   as LB (concat)
import           Data.Modbus.Types      (Packet, decode, encode)

class Serial serial where
  flush :: serial -> IO ()
  recv  :: serial -> Int -> IO ByteString
  send  :: serial -> ByteString -> IO Int

recvPacket :: (Binary a, Serial serial) => serial -> TVar ByteString -> IO (Packet a)
recvPacket port bufTvar = do
  !r <- getPacketData bufTvar
  case r of
    Just p ->
      case decode (fromStrict p) of
        Nothing -> recvPacket port bufTvar
        Just p0 -> return p0
    Nothing -> do
      bs <- recv port 20
      setPacketData bufTvar bs
      recvPacket port bufTvar

getPacketData :: TVar ByteString -> IO (Maybe ByteString)
getPacketData bufTvar = atomically $ do
  buf <- B.dropWhile (/=0x3A) <$> readTVar bufTvar
  if hasPacket buf then do
    writeTVar bufTvar $! B.dropWhile (/=0x0D) buf
    return $ Just $ removePrefix $ B.takeWhile (/=0x0D) buf
  else return Nothing

  where hasPacket :: ByteString -> Bool
        hasPacket = B.elem 0x0A

-- remove packet prefix :
removePrefix :: ByteString -> ByteString
removePrefix bs
  | B.head bs == 0x3A = removePrefix $ B.drop 1 bs
  | B.elem 0x3A bs    = removePrefix $ B.dropWhile (/=0x3A) bs
  | otherwise          = bs

setPacketData :: TVar ByteString -> ByteString -> IO ()
setPacketData bufTvar bs = atomically $ do
  buf <- readTVar bufTvar
  writeTVar bufTvar $! buf <> bs

sendPacket :: (Binary a, Serial serial) => serial -> Packet a -> IO ()
sendPacket port p = do
  sendBuffer port $ toStrict $ LB.concat [":", encode p, "\r\n"]
  flush port

sendBuffer :: Serial serial => serial -> ByteString -> IO ()
sendBuffer _ "" = pure ()
sendBuffer port bs = do
  v <- send port bs
  when (v < B.length bs) $ sendBuffer port $ B.drop v bs
