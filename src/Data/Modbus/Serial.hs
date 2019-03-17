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
                                                    dropWhile, elem, takeWhile)
import           Data.Modbus.Types           (Packet, decode, encode)
import           System.Hardware.Serialport  (SerialPort, recv, send)

recvPacket :: Binary a => SerialPort -> TVar ByteString -> IO (Packet a)
recvPacket port bufTvar = do
  r <- getPacketData bufTvar
  case r of
    Just p -> do
      pkt <- decode p
      case pkt of
        Nothing -> recvPacket port bufTvar
        Just p0 -> return p0
    Nothing -> do
      bs <- recv port 20
      setPacketData bufTvar $ fromStrict bs
      recvPacket port bufTvar

getPacketData :: TVar ByteString -> IO (Maybe ByteString)
getPacketData bufTvar = atomically $ do
  buf <- readTVar bufTvar
  if hasPacket buf then do
    writeTVar bufTvar $ LB.dropWhile (/=0x0A) buf
    return $ Just $ LB.drop 1 $ LB.takeWhile (/=0x0D) buf
  else return Nothing

  where hasPacket :: ByteString -> Bool
        hasPacket = LB.elem 0x0A . LB.dropWhile (/=0x3A)

setPacketData :: TVar ByteString -> ByteString -> IO ()
setPacketData bufTvar bs = atomically $ do
  buf <- readTVar bufTvar
  writeTVar bufTvar (LB.dropWhile (/=0x3A) $ buf `LB.append` bs)

sendPacket :: Binary a => SerialPort -> Packet a -> IO ()
sendPacket port p = sendBuffer port $ toStrict $ LB.concat [":", encode p, "\r\n"]

sendBuffer :: SerialPort -> B.ByteString -> IO ()
sendBuffer port bs = do
  v <- send port bs
  when (v < B.length bs) $ sendBuffer port $ B.drop v bs
