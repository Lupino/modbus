{-# LANGUAGE OverloadedStrings #-}

module Data.Modbus.Serial
  ( recvPacket
  , sendPacket
  ) where

import           Control.Concurrent.STM.TVar (TVar, readTVar, writeTVar)
import           Control.Monad               (when)
import           Control.Monad.STM           (atomically)
import           Data.Binary                 (Binary (..))
import qualified Data.ByteString             as B (ByteString, drop, length)
import           Data.ByteString.Lazy        (ByteString, fromStrict, toStrict)
import qualified Data.ByteString.Lazy        as LB (append, concat, drop,
                                                    dropWhile, elem, head,
                                                    takeWhile)
import           Data.Modbus.Types           (Packet, decode, encode)
import           System.Hardware.Serialport  (SerialPort, flush, recv, send)

recvPacket :: Binary a => SerialPort -> TVar ByteString -> IO (Packet a)
recvPacket port bufTvar = do
  r <- getPacketData bufTvar
  case r of
    Just p ->
      case decode p of
        Nothing -> recvPacket port bufTvar
        Just p0 -> return p0
    Nothing -> do
      bs <- recv port 20
      setPacketData bufTvar $ fromStrict bs
      recvPacket port bufTvar

getPacketData :: TVar ByteString -> IO (Maybe ByteString)
getPacketData bufTvar = atomically $ do
  buf <- LB.dropWhile (/=0x3A) <$> readTVar bufTvar
  if hasPacket buf then do
    writeTVar bufTvar $ LB.dropWhile (/=0x0D) buf
    return $ Just $ removePrefix $ LB.takeWhile (/=0x0D) buf
  else return Nothing

  where hasPacket :: ByteString -> Bool
        hasPacket = LB.elem 0x0A

-- remove packet prefix :
removePrefix :: ByteString -> ByteString
removePrefix bs
  | LB.head bs == 0x3A = removePrefix $ LB.drop 1 bs
  | LB.elem 0x3A bs    = removePrefix $ LB.dropWhile (/=0x3A) bs
  | otherwise          = bs

setPacketData :: TVar ByteString -> ByteString -> IO ()
setPacketData bufTvar bs = atomically $ do
  buf <- readTVar bufTvar
  writeTVar bufTvar (LB.dropWhile (/=0x3A) $ buf `LB.append` bs)

sendPacket :: Binary a => SerialPort -> Packet a -> IO ()
sendPacket port p = do
  sendBuffer port $ toStrict $ LB.concat [":", encode p, "\r\n"]
  flush port

sendBuffer :: SerialPort -> B.ByteString -> IO ()
sendBuffer _ "" = pure ()
sendBuffer port bs = do
  v <- send port bs
  when (v < B.length bs) $ sendBuffer port $ B.drop v bs
