{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Data.Modbus.Memory
  ( Memory (..)
  , emptyMemory
  , trainMemory
  , runMemory
  , fromMemoryField
  , toMemoryField
  ) where

import           Data.Aeson                 (FromJSON (..), ToJSON (..), object,
                                             withObject, (.:), (.=))
import           Data.Aeson.Types           (Pair)
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as HM
import           Data.Modbus.Types.Internal
import           Data.Modbus.Types.Request  (Request)
import qualified Data.Modbus.Types.Request  as Req
import           Data.Modbus.Types.Response (Response)
import qualified Data.Modbus.Types.Response as Res

data Memory = Memory
  { memoryCoils      :: !(HashMap Address Bit)
  , memoryInputStats :: !(HashMap Address Bit)
  , memoryRegs       :: !(HashMap Address Value)
  , memoryInputRegs  :: !(HashMap Address Value)
  }
  deriving (Show)

emptyMemory :: Memory
emptyMemory = Memory
  { memoryCoils      = HM.empty
  , memoryInputStats = HM.empty
  , memoryRegs       = HM.empty
  , memoryInputRegs  = HM.empty
  }

updateRegsOrCoils :: Address -> [a] -> HashMap Address a -> HashMap Address a
updateRegsOrCoils addr xs = HM.union (HM.fromList $ mkRegOrCoilMap addr xs)

updateMemoryCoils :: Address -> [Bit] -> Memory -> Memory
updateMemoryCoils addr xs m = m {memoryCoils = updateRegsOrCoils addr xs (memoryCoils m)}

updateMemoryInputStats :: Address -> [Bit] -> Memory -> Memory
updateMemoryInputStats addr xs m = m {memoryInputStats = updateRegsOrCoils addr xs (memoryInputStats m)}

updateMemoryRegs :: Address -> [Value] -> Memory -> Memory
updateMemoryRegs addr xs m = m {memoryRegs = updateRegsOrCoils addr xs (memoryRegs m)}

updateMemoryInputRegs :: Address -> [Value] -> Memory -> Memory
updateMemoryInputRegs addr xs m = m {memoryInputRegs = updateRegsOrCoils addr xs (memoryInputRegs m)}

trainMemory :: Request -> Response -> Memory -> Memory
trainMemory (Req.ReadCoils s _) (Res.ReadCoils cols)           = updateMemoryCoils s cols
trainMemory (Req.ReadInputStats s _) (Res.ReadInputStats cols) = updateMemoryInputStats s cols
trainMemory (Req.ReadRegs s _) (Res.ReadRegs cols)             = updateMemoryRegs s cols
trainMemory (Req.ReadInputRegs s _) (Res.ReadInputRegs cols)   = updateMemoryInputRegs s cols
trainMemory (Req.WriteCoil _ _) (Res.WriteCoil s1 n1)          = updateMemoryCoils s1 [n1]
trainMemory (Req.WriteReg _ _) (Res.WriteReg s1 n1)            = updateMemoryRegs s1 [n1]
trainMemory (Req.WriteCoils s cols) (Res.WriteCoils _ _)       = updateMemoryCoils s cols
trainMemory (Req.WriteRegs s cols) (Res.WriteRegs _ _)         = updateMemoryRegs s cols
trainMemory _ _                                                = id

getMemoryValueArray :: a -> Address -> NumReg -> HashMap Address a -> [a]
getMemoryValueArray v addr n hm = map (flip (HM.lookupDefault v) hm) [addr..addr+n-1]

runMemory :: Memory -> Request -> (Memory, Response)
runMemory m (Req.ReadCoils s n)      = (m, Res.ReadCoils $ getMemoryValueArray False s n $ memoryCoils m)
runMemory m (Req.ReadInputStats s n) = (m, Res.ReadInputStats $ getMemoryValueArray False s n $ memoryInputStats m)
runMemory m (Req.ReadRegs s n)       = (m, Res.ReadRegs $ getMemoryValueArray 0 s n $ memoryRegs m)
runMemory m (Req.ReadInputRegs s n)  = (m, Res.ReadInputRegs $ getMemoryValueArray 0 s n $ memoryInputRegs m)
runMemory m (Req.WriteCoil s n)      = (updateMemoryCoils s [n] m, Res.WriteCoil s n)
runMemory m (Req.WriteReg s n)       = (updateMemoryRegs s [n] m, Res.WriteReg s n)
runMemory m (Req.WriteCoils s cols)  = (updateMemoryCoils s cols m, Res.WriteCoils s (fromIntegral $ length cols))
runMemory m (Req.WriteRegs s cols)   = (updateMemoryRegs s cols m, Res.WriteRegs s (fromIntegral $ length cols))

data MemoryField =
  Coil Address Bit [Pair]
  | InputStat Address Bit [Pair]
  | Reg Address Value [Pair]
  | InputReg Address Value [Pair]

instance ToJSON MemoryField where
  toJSON (Coil addr bit ex)      = object $ ["type" .= ("coil" :: String), "value" .= bit, "addr" .= addr] ++ ex
  toJSON (InputStat addr bit ex) = object $ ["type" .= ("input_stat" :: String), "value" .= bit, "addr" .= addr] ++ ex
  toJSON (Reg addr val ex)       = object $ ["type" .= ("reg" :: String), "value" .= val, "addr" .= addr] ++ ex
  toJSON (InputReg addr val ex)  = object $ ["type" .= ("input_reg" :: String), "value" .= val, "addr" .= addr] ++ ex

instance FromJSON MemoryField where
  parseJSON = withObject "MemoryField" $ \o -> do
    tp <- o .: "type"
    addr <- o .: "addr"
    case tp of
      "coil" -> do
        bit <- o .: "value"
        return $ Coil addr bit []
      "input_stat" -> do
        bit <- o .: "value"
        return $ InputStat addr bit []
      "reg" -> do
        val <- o .: "value"
        return $ Reg addr val []
      "input_reg" -> do
        val <- o .: "value"
        return $ InputReg addr val []
      _ -> fail $ "MemoryField no such type " ++ tp

toMemoryField :: Memory -> (Address -> [Pair]) -> [MemoryField]
toMemoryField Memory {..} f =
  map (doMap Coil) (HM.toList memoryCoils)
  ++ map (doMap InputStat) (HM.toList memoryInputStats)
  ++ map (doMap Reg) (HM.toList memoryRegs)
  ++ map (doMap InputReg) (HM.toList memoryInputRegs)
  where doMap :: (Address -> a -> [Pair] -> MemoryField) -> (Address, a) -> MemoryField
        doMap f0 (addr, v) = f0 addr v (f addr)

fromMemoryField :: [MemoryField] -> Memory
fromMemoryField fields = go fields emptyMemory
  where go :: [MemoryField] -> Memory -> Memory
        go [] m = m
        go (Coil addr bit _:xs) m      = go xs (m {memoryCoils = HM.insert addr bit (memoryCoils m)})
        go (InputStat addr bit _:xs) m = go xs (m {memoryInputStats = HM.insert addr bit (memoryInputStats m)})
        go (Reg addr val _:xs) m       = go xs (m {memoryRegs = HM.insert addr val (memoryRegs m)})
        go (InputReg addr val _:xs) m  = go xs (m {memoryInputRegs = HM.insert addr val (memoryInputRegs m)})
