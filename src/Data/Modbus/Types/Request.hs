module Data.Modbus.Types.Request
  ( Request (..)
  ) where

import           Control.Monad              (replicateM)
import           Data.Binary                (Binary (..), getWord8, putWord8)
import           Data.Binary.Get            (getWord16be)
import           Data.Binary.Put            (putWord16be)
import           Data.Modbus.Types.Internal

data Request =
  ReadCoils Address NumReg        -- Read Coils (Output) Status 0xxxx
  | ReadInputStats Address NumReg -- Read Input Status (Discrete Inputs) 1xxxx
  | ReadRegs Address NumReg       -- Read Holding Registers 4xxxx
  | ReadInputRegs Address NumReg  -- Read Input Registers 3xxxx
  | WriteCoil Address Bit         -- Write Single Coil (Output) 0xxxx
  | WriteReg Address Value        -- Preset Single Register 4xxxx
  | WriteCoils Address [Bit]      -- Write Multiple Coils (Outputs) 0xxxx
  | WriteRegs Address [Value]     -- Write block of contiguous registers 4xxxx

  deriving (Show)

instance Binary Request where
  get = do
    fc <- getWord8
    field1 <- getWord16be
    field2 <- getWord16be
    case fc of
      0x01 -> pure $ ReadCoils field1 field2
      0x02 -> pure $ ReadInputStats field1 field2
      0x03 -> pure $ ReadRegs field1 field2
      0x04 -> pure $ ReadInputRegs field1 field2
      0x05 -> pure $ WriteCoil field1 $ field2 == 0xFF00
      0x06 -> pure $ WriteReg field1 field2
      0x0F -> do
        ln <- fromIntegral <$> getWord8
        WriteCoils field1 . concatMap toBitList <$> replicateM ln getWord8
      0x10 -> do
        _ <- getWord8
        WriteRegs field1 <$> replicateM (fromIntegral field2) getWord16be

      _ -> fail "no support function code"

  put (ReadCoils field1 field2) = do
    putWord8 0x01
    putWord16be field1
    putWord16be field2
  put (ReadInputStats field1 field2) = do
    putWord8 0x02
    putWord16be field1
    putWord16be field2
  put (ReadRegs field1 field2) = do
    putWord8 0x03
    putWord16be field1
    putWord16be field2
  put (ReadInputRegs field1 field2) = do
    putWord8 0x04
    putWord16be field1
    putWord16be field2
  put (WriteCoil field1 field2) = do
    putWord8 0x05
    putWord16be field1
    putWord16be $ if field2 then 0xFF00 else 0x0000
  put (WriteReg field1 field2) = do
    putWord8 0x06
    putWord16be field1
    putWord16be field2
  put (WriteCoils field1 cols) = do
    putWord8 0x0F
    putWord16be field1
    putWord16be . fromIntegral $ length cols
    putWord8 . fromIntegral $ length cols `div` 8
    mapM_ putWord8 $ toWordList' cols
  put (WriteRegs field1 cols) = do
    putWord8 0x10
    putWord16be field1
    putWord16be . fromIntegral $ length cols
    putWord8 . fromIntegral $ length cols * 2
    mapM_ putWord16be cols
