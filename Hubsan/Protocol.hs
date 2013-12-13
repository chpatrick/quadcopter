{-# LANGUAGE RecordWildCards #-}

module Hubsan.Protocol where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Trans
import Control.Monad.Trans.Error
import Data.Bits
import Data.Binary.Get as Get
import qualified Data.ByteString as BS
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy as BSL
import Data.Monoid
import Data.Word

import A7105
import A7105.Enums
import Util

checksum :: BSL.ByteString -> Word8
checksum = negate . BSL.foldl (+) 0

checksummed :: Builder -> Builder
checksummed pb
  = lazyByteString bytes <> word8 (checksum bytes) 
    where bytes = toLazyByteString pb 

bindPacket :: Word8 -> Word8 -> Word32 -> Word32 -> Builder
bindPacket state chan sessID txID = checksummed $
  word8 state <>
  word8 chan <>
  word32BE sessID <>
  byteString mysteryConstants <> 
  word32BE txID
    where
      mysteryConstants = BS.pack [ 0x08, 0xe4, 0xea, 0x9e, 0x50 ]

data ControlData = ControlData
  { throttle :: Word8
  , rudder   :: Word8
  , elevator :: Word8
  , aileron  :: Word8
  , lights   :: Bool
  , flips    :: Bool
  }

dataPacket :: ControlData -> Word32 -> Builder
dataPacket ControlData {..} txID = checksummed $ 
  word8 0x20 <>
  word8 0x00 <>
  word8 throttle <>
  word8 0x00 <>
  word8 rudder <>
  word8 0x00 <>
  word8 elevator <>
  word8 0x00 <>
  word8 aileron <>
  word8 settings <>
  word8 0x64 <>
  word32BE txID
    where
      settings = flag 3 flips $ flag 2 lights $ bit 1
      flag b f = if f then (`setBit`b) else id

calibrate :: A7105 a => a -> Word8 -> Register -> Int -> ErrorT String IO ()
calibrate a7105 cb reg maskBit = do
  success <- liftIO $ do
    writeReg Calc cb a7105
    t <- getMillis
    fix $ \checkReg -> do
      t' <- getMillis
      if t' - t > 500
        then
          return False
        else do
          r <- readReg Calc a7105
          if r == 0 then return True else checkReg
  unless success $ throwError "Calibration failed."
  calibration <- liftIO $ readReg reg a7105
  unless (testBit calibration maskBit) $ throwError "Calibration failed."

initialize :: A7105 a => a -> ErrorT String IO ()
initialize a7105 = do
  liftIO $ a7105
    .> reset
    >. writeID 0x55201041
    >. writeReg ModeControl 0x63
    >. writeReg FifoI 0x0f
    >. writeReg Clock 0x05
    >. writeReg DataRate 0x04
    >. writeReg TxII 0x2b
    >. writeReg Rx 0x62
    >. writeReg RxGainI 0x80
    >. writeReg RxGainIV 0x0A
    >. writeReg CodeI 0x07
    >. writeReg CodeII 0x17
    >. writeReg RxDemTestI 0x47

    >. strobe Standby

  -- calibrate IF filter bank
  calibrate a7105 0x01 VcoCurcal fbcfBit

  -- calibrate channel 00
  liftIO $ a7105 .> writeReg PllI 0x00
  calibrate a7105 0x02 VcoSbcalI vbcfBit

  -- calibrate channel A0
  liftIO $ a7105 .> writeReg PllI 0xA0
  calibrate a7105 0x02 VcoSbcalI vbcfBit

  liftIO $ a7105
    .> setPower power0dBm
    >. strobe Standby

sendPacket :: A7105 a => Channel -> Builder -> a -> IO ()
sendPacket chan pb = writeData (toLazyByteString pb) chan

data BindState = Stage1A | Stage1B | Stage2 | Stage3
  deriving ( Enum, Ord, Eq, Show )

bind :: A7105 a => Channel -> Word32 -> Word32 -> a -> IO ()
bind chan sessID txID a7105
  = bindStep Stage1A
    where
      bindStep stage = do
        let
          state = case stage of
            -- stage 1
            Stage1A -> 1
            Stage1B -> 3
            -- stage 2
            Stage2 -> 1
            -- stage 3
            Stage3 -> 9
        a7105
          .> strobe Standby
          >. sendPacket chan (bindPacket state chan sessID txID)
          >. waitWrite 20
          >. strobe RxState

        status <- a7105 .> readReg Mode
        if testBit status 0
          then bindStep (if stage < Stage3 then Stage1A else Stage2) -- start again
          else do
            response <- a7105 .> readData 16
            when (stage == Stage1B) $ do
              let id = Get.runGet (Get.skip 2 >> getWord32be) response
              a7105 .> writeID id
            if stage < Stage3
              then bindStep (succ stage)
              else unless (BSL.index response 1 == 9) (bindStep Stage3)
