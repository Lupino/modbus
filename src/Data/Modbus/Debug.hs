{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Data.Modbus.Debug
  ( showMemory
  , showRequest
  , showResponse
  ) where

import qualified Data.HashMap.Strict        as HM (toList)
import           Data.Maybe                 (fromMaybe)
import           Data.Modbus.AddrMap        (AddrMap, Code (..), lookupCode)
import           Data.Modbus.Memory         (Memory (..))
import           Data.Modbus.Types          (Address, Request, Response,
                                             mkRegOrCoilMap)
import qualified Data.Modbus.Types.Request  as Req
import qualified Data.Modbus.Types.Response as Res
import           Data.Text                  (Text, append, intercalate, pack)

showOne :: Show a => AddrMap -> Text -> Address -> a -> Text
showOne addrMap h s n = h `append` " " `append` formatCoilOrReg addrMap s n

showRequest :: AddrMap -> Request -> Text
showRequest addrMap (Req.ReadCoils s n)      = showOne addrMap "ReqReadCoils" s n
showRequest addrMap (Req.ReadInputStats s n) = showOne addrMap "ReqReadInputStats" s n
showRequest addrMap (Req.ReadRegs s n)       = showOne addrMap "ReqReadRegs" s n
showRequest addrMap (Req.ReadInputRegs s n)  = showOne addrMap "ReqReadInputRegs" s n
showRequest addrMap (Req.WriteCoil s n)      = showOne addrMap "ReqWriteCoil" s n
showRequest addrMap (Req.WriteReg s n)       = showOne addrMap "ReqWriteReg" s n
showRequest addrMap (Req.WriteCoils s cols)  = showOne addrMap "ReqWriteCoils" s cols
showRequest addrMap (Req.WriteRegs s cols)   = showOne addrMap "ReqWriteRegs" s cols

showArray :: Show a => AddrMap -> Text -> Address -> [a] -> Text
showArray addrMap h addr xs = showMemoryArray addrMap h $ mkRegOrCoilMap addr xs

showResponse :: AddrMap -> Request -> Response -> Text
showResponse addrMap (Req.ReadCoils s _) (Res.ReadCoils cols)           = showArray addrMap "ResReadCoils" s cols
showResponse addrMap (Req.ReadInputStats s _) (Res.ReadInputStats cols) = showArray addrMap "ResReadInputStats" s cols
showResponse addrMap (Req.ReadRegs s _) (Res.ReadRegs cols)             = showArray addrMap "ResReadRegs" s cols
showResponse addrMap (Req.ReadInputRegs s _) (Res.ReadInputRegs cols)   = showArray addrMap "ResReadInputRegs" s cols
showResponse addrMap (Req.WriteCoil _ _) (Res.WriteCoil s1 n1)          = showOne addrMap "ResWriteCoil" s1 n1
showResponse addrMap (Req.WriteReg _ _) (Res.WriteReg s1 n1)            = showOne addrMap "ResWriteReg" s1 n1
showResponse addrMap (Req.WriteCoils _ _) (Res.WriteCoils s1 n1)        = showOne addrMap "ResWriteCoils" s1 n1
showResponse addrMap (Req.WriteRegs _ _) (Res.WriteRegs s1 n1)          = showOne addrMap "ResWriteRegs" s1 n1
showResponse _ r s                                                      = "Error " `append` showText r `append` " " `append` showText s

showMemoryArray :: Show a => AddrMap -> Text -> [(Address, a)] -> Text
showMemoryArray addrMap h cols =
  h `append` "\n    " `append` intercalate "\n    " (map (uncurry (formatCoilOrReg addrMap)) cols)

formatCoilOrReg :: Show a => AddrMap -> Address -> a -> Text
formatCoilOrReg addrMap k v = "(" `append` p `append` ", " `append` r `append` ", " `append` showText v `append` ")"
  where c = lookupCode k addrMap
        (p, r) = case c of
                   Nothing -> (showText k, showText k)
                   Just c' -> (name c', fromMaybe (name c') $ comment c')

showText :: Show a => a -> Text
showText = pack . show

showMemory :: AddrMap -> Memory -> Text
showMemory addrMap Memory {..} = intercalate "\n"
  [ showMemoryArray addrMap "Memory Coils:" $ HM.toList memoryCoils
  , showMemoryArray addrMap "Memory InputStats:" $ HM.toList memoryInputStats
  , showMemoryArray addrMap "Memory Regs:" $ HM.toList memoryRegs
  , showMemoryArray addrMap "Memory InputRegs" $ HM.toList memoryInputRegs
  ]
