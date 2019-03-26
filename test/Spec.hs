{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad              (unless)
import           Data.ByteString.Lazy       (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LBC (unpack)
import           Data.Modbus
import           Data.Modbus.Debug
import qualified Data.Modbus.Types.Request  as Req
import qualified Data.Modbus.Types.Response as Res
import qualified Data.Text                  as T (unpack)

main :: IO ()
main = do
  let req1 = Req.ReadCoils 1000 8
      res1 = Res.ReadCoils [False, True, False, True, False, True, False, True]
      req4 = Req.ReadInputRegs 500 2
      res4 = Res.ReadInputRegs [100, 500]
      reqf = Req.WriteCoils 500 [False, True, False, True, False, True, False, True]
      resf = Res.WriteCoils 500 8
  testPacket (encode (packet 0 req1)) (encode (packet 0 res1))
  testPacket "000209F00020E5" "00020400000800F2"
  testPacket "000209F00020E5" "00020400000800F2"
  testPacket "000310D100011B" "0003020009F2"
  testPacket (encode (packet 0 req4)) (encode (packet 0 res4))
  testPacket "00050A00FF00F2" "000508340000BF"
  testPacket (encode (packet 0 reqf)) (encode (packet 0 resf))
  testPacket "00109786000306001C000107E3C3" "001097860003D0"


testPacket :: ByteString -> ByteString -> IO ()
testPacket bs0 bs1  = do
  case decode bs0 of
    Nothing  -> fail $ "error format " ++ LBC.unpack bs0
    Just pkt0 -> do
      putStrLn . T.unpack $ showRequest [] $ packetData pkt0
      unless (lrcCheck pkt0) $ fail $ "Lrc check failed " ++ LBC.unpack bs0
      let bs00 = encode pkt0
      unless (bs0 == bs00)
        $ fail
        $ "Except " ++ LBC.unpack bs0 ++ " Got: " ++ LBC.unpack bs00

      case decode bs1 of
        Nothing -> fail $ "error format " ++ LBC.unpack bs1
        Just pkt1 -> do
          putStrLn . T.unpack $ showResponse [] (packetData pkt0) (packetData pkt1)
          unless (lrcCheck pkt1) $ fail $ "Lrc check failed " ++ LBC.unpack bs1
          let bs10 = encode pkt1
          unless (bs1 == bs10)
            $ fail
            $ "Except " ++ LBC.unpack bs1 ++ " Got: " ++ LBC.unpack bs10
