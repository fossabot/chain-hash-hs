module Crypto.ChainHash.Util where

import           Control.Arrow
import           Control.Exception
import qualified Data.ByteString   as B
import           Numeric           (readHex)
import           Text.Printf

data ChainHashException =
  InvalidChunkException Int
                        B.ByteString
                        B.ByteString

instance Exception ChainHashException

instance Show ChainHashException where
  show (InvalidChunkException p ha hb) =
    printf
      "Invalid chunk beginning at position %d. Hash: Expected %s, Saw %s"
      p
      (hexify ha)
      (hexify hb)

hexify :: B.ByteString -> String
hexify = (printf "%02x" =<<) . B.unpack

stringToHex :: String -> Integer
stringToHex s = fst (head (readHex s))

unhexify :: String -> B.ByteString
unhexify [] = B.empty
unhexify [_] =
  error "Cannot unhexify a string with an odd number of characters."
unhexify s = (((take 2 >>> stringToHex >>> fromInteger) &&& (drop 2 >>> unhexify)) >>> uncurry B.cons) s
