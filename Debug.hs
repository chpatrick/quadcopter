import qualified Data.ByteString.Lazy as BSL
import Control.Applicative
import Control.Monad
import Data.List
import Numeric
import Text.Printf

import A7105
import Hubsan.Protocol

data MockA7105 = MockA7105

readHex' h = case readHex h of
  [ ( v, _ ) ] -> Just v
  _ -> Nothing

getHex = do
  l <- getLine
  case readHex' l of
    Just v -> return v
    _ -> getHex

getHexes = do
  l <- getLine
  let vs = mapM (readHex') (words l)
  case vs of
    Just vs -> return vs
    Nothing -> getHexes

instance A7105 MockA7105 where
  writeReg reg val _ = printf "writeReg %s %02x\n" (show reg) val
  readReg reg _ = printf "readReg %s: " (show reg) >> getHex
  writeData vs chan _ = printf "writeData %02x  - %s\n" chan $ intercalate " " $ map (printf "%02X") (BSL.unpack vs)
  readData chan _ = printf "readData %02x: " chan >> BSL.pack <$> getHexes
  writeID id _ = printf "writeID %04x\n" id
  strobe state _ = printf "strobe %s\n" (show state)
