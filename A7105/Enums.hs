module A7105.Enums where

import Data.Word

data State
  = Sleep
  | Idle
  | Standby
  | PLL
  | RxState
  | TxState
  | RstWrptr
  | RstRdptr
    deriving ( Show, Eq, Ord )

instance Enum State where
  fromEnum Sleep    = 0x80
  fromEnum Idle     = 0x90
  fromEnum Standby  = 0xA0
  fromEnum PLL      = 0xB0
  fromEnum RxState  = 0xC0
  fromEnum TxState  = 0xD0
  fromEnum RstWrptr = 0xE0
  fromEnum RstRdptr = 0xF0
  toEnum 0x80 = Sleep
  toEnum 0x90 = Idle
  toEnum 0xA0 = Standby
  toEnum 0xB0 = PLL
  toEnum 0xC0 = RxState
  toEnum 0xD0 = TxState
  toEnum 0xE0 = RstWrptr
  toEnum 0xF0 = RstRdptr

data Register
  = Mode
  | ModeControl
  | Calc
  | FifoI
  | FifoII
  | FifoData
  | IdData
  | RcOscI
  | RcOscII
  | RcOscIII
  | Ck0Pin
  | Gpio1Pin1
  | Gpio2PinII
  | Clock
  | DataRate
  | PllI
  | PllII
  | PllIII
  | PllIV
  | PllV
  | TxI
  | TxII
  | DelayI
  | DelayII
  | Rx
  | RxGainI
  | RxGainII
  | RxGainIII
  | RxGainIV
  | RssiThold
  | Adc
  | CodeI
  | CodeII
  | CodeIII
  | IfCalibI
  | IfCalibII
  | VcoCurcal
  | VcoSbcalI
  | VcoSbcalII
  | BatteryDet
  | TxTest
  | RxDemTestI
  | RxDemTestII
  | Cpc
  | XtalTest
  | PllTest
  | VcoTestI
  | VcoTestII
  | Ifat
  | Rscale
  | FilterTest
    deriving ( Show, Eq, Ord, Enum )

fbcfBit :: Int
fbcfBit = 4

vbcfBit :: Int
vbcfBit = 3

data PowerLevel = PowerLevel { pac :: Word8, tbg :: Word8 }
  deriving ( Show, Ord, Eq )

power1dBm   = PowerLevel 3 7
power0dBm   = PowerLevel 1 7
power_10Dbm = PowerLevel 1 3
power_20Dbm = PowerLevel 0 1
