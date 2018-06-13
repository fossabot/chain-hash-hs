module Crypto.ChainHash.Decode (decode) where

import           Control.Exception
import           Crypto.ChainHash.Util
import qualified Crypto.Hash.SHA256    as Sha256
import qualified Data.ByteString       as B
import           System.IO

handleChunk ::
     Handle
  -> Handle
  -> B.ByteString
  -> Integer
  -> Integer
  -> Integer
  -> IO (Either ChainHashException ())
handleChunk inHandle outHandle currHash maxSize position chunkSize
  | position >= maxSize = return $ Right ()
  | otherwise = do
    chunk <- B.hGet inHandle (fromInteger chunkSize + 32)
    if currHash /= Sha256.hash chunk
      then return
             (Left (InvalidChunkException position currHash (Sha256.hash chunk)))
      else do
        let (dataBytes, hashBytes) = B.splitAt (fromInteger chunkSize) chunk
        B.hPut outHandle dataBytes
        handleChunk
          inHandle
          outHandle
          hashBytes
          maxSize
          (position + chunkSize + 32)
          chunkSize

decode :: FilePath -> FilePath -> B.ByteString -> Integer -> IO ()
decode inFilePath outFilePath hashStr chunkSize = do
  inHandle <- openBinaryFile inFilePath ReadMode
  outHandle <- openBinaryFile outFilePath WriteMode
  totalSize <- hFileSize inHandle
  ok <- handleChunk inHandle outHandle hashStr totalSize 0 chunkSize
  hClose inHandle
  hClose outHandle
  either throwIO return ok
