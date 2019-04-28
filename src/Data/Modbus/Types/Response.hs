module Data.Modbus.Types.Response
  ( Response (..)
  ) where

import           Control.Monad              (replicateM)
import           Data.Binary                (Binary (..), getWord8, putWord8)
import           Data.Binary.Get            (getWord16be)
import           Data.Binary.Put            (putWord16be)
import           Data.Modbus.Types.Internal


data Response =
  ReadCoils ![Bit]              -- Read Coils (Output) Status 0xxxx
  | ReadInputStats ![Bit]       -- Read Input Status (Discrete Inputs) 1xxxx
  | ReadRegs ![Value]           -- Read Holding Registers 4xxxx
  | ReadInputRegs ![Value]      -- Read Input Registers 3xxxx
  | WriteCoil !Address !Bit     -- Write Single Coil (Output) 0xxxx
  | WriteReg !Address !Value    -- Preset Single Register 4xxxx
  | WriteCoils !Address !NumReg -- Write Multiple Coils (Outputs) 0xxxx
  | WriteRegs !Address !NumReg  -- Write block of contiguous registers 4xxxx

  deriving (Show)

instance Binary Response where
  get = do
    fc <- getWord8
    case fc of
      0x01 -> do
        size <- fromIntegral <$> getWord8
        ReadCoils . concatMap toBitList <$> replicateM size getWord8
      0x02 -> do
        size <- fromIntegral <$> getWord8
        ReadInputStats . concatMap toBitList <$> replicateM size getWord8
      0x03 -> do
        size <- fromIntegral <$> getWord8
        ReadRegs <$> replicateM (size `div` 2) getWord16be
      0x04 -> do
        size <- fromIntegral <$> getWord8
        ReadInputRegs <$> replicateM (size `div` 2) getWord16be
      0x05 -> do
        field1 <- getWord16be
        WriteCoil field1 . (== 0xFF00) <$> getWord16be
      0x06 -> do
        field1 <- getWord16be
        WriteReg field1 <$> getWord16be
      0x0F -> do
        field1 <- getWord16be
        WriteCoils field1 <$> getWord16be
      0x10 -> do
        field1 <- getWord16be
        WriteRegs field1 <$> getWord16be

      _ -> fail "no support function code"

  put (ReadCoils cols) = do
    putWord8 0x01
    putWord8 . fromIntegral $ length cols `div` 8
    mapM_ putWord8 $ toWordList' cols
  put (ReadInputStats cols) = do
    putWord8 0x02
    putWord8 . fromIntegral $ length cols `div` 8
    mapM_ putWord8 $ toWordList' cols
  put (ReadRegs cols) = do
    putWord8 0x03
    putWord8 . fromIntegral $ length cols * 2
    mapM_ putWord16be cols
  put (ReadInputRegs cols) = do
    putWord8 0x04
    putWord8 . fromIntegral $ length cols * 2
    mapM_ putWord16be cols
  put (WriteCoil field1 field2) = do
    putWord8 0x05
    putWord16be field1
    putWord16be $ if field2 then 0xFF00 else 0x0000
  put (WriteReg field1 field2) = do
    putWord8 0x06
    putWord16be field1
    putWord16be field2
  put (WriteCoils field1 field2) = do
    putWord8 0x0F
    putWord16be field1
    putWord16be field2
  put (WriteRegs field1 field2) = do
    putWord8 0x10
    putWord16be field1
    putWord16be field2
