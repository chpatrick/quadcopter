module A7105 (A7105(..), readData, setPower, reset, waitWrite, Channel, ID) where

import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy as BSL
import Data.Bits
import Data.Word

import A7105.Enums
import Util

type Channel = Word8
type ID = Word32

class A7105 a where
  writeReg  :: Register -> Word8 -> a -> IO ()
  readReg   :: Register -> a -> IO Word8
  writeData :: BSL.ByteString -> Channel -> a -> IO ()
  writeID   :: ID -> a -> IO ()
  strobe    :: State -> a -> IO () 

readData :: A7105 a => Int -> a -> IO BSL.ByteString
readData len a7105 = do
  a7105 .> strobe RstRdptr
  BSL.pack <$> replicateM len (a7105 .> readReg FifoData)

setPower :: A7105 a => PowerLevel -> a -> IO ()
setPower p = writeReg TxTest ((pac p `shiftL` 3) .|. tbg p)

reset :: A7105 a => a -> IO ()
reset = writeReg Mode 0x00

waitWrite :: A7105 a => Int -> a -> IO ()
waitWrite timeout a7105 = do
  let
    poll c
      = case c of
        0 -> return ()
        n -> do
          status <- a7105 .> readReg Mode
          if testBit status 0 then poll (c - 1) else return ()
  poll timeout
