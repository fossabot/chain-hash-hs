module Crypto.ChainHash.Encode
  ( encode
  ) where

import           Control.Arrow
import qualified Crypto.Hash.SHA256  as Sha256
import qualified Data.ByteString     as B
import           System.IO

readData :: Handle -> Int -> Int -> IO B.ByteString
readData handle start size = do
  hSeek handle AbsoluteSeek (toInteger start)
  B.hGet handle size

calculateChunks :: Handle -> Int -> IO [(Int, Int)]
calculateChunks handle chunkSize = do
  totalSize <- fromInteger <$> hFileSize handle
  return $
    map (\n -> (n, min chunkSize (totalSize - n))) [0,chunkSize .. totalSize]

loadDataBackToFront :: Handle -> Int -> IO [B.ByteString]
loadDataBackToFront handle chunkSize = do
  positions <- calculateChunks handle chunkSize
  mapM (uncurry (readData handle)) (reverse positions)

hashScan :: B.ByteString -> B.ByteString -> B.ByteString
hashScan prev curr = Sha256.hash (B.append curr prev)

hashSequence :: [B.ByteString] -> [B.ByteString]
hashSequence = ((head >>> Sha256.hash) &&& tail) >>> uncurry (scanl hashScan)

putNextChunk :: Handle -> Handle -> Int -> B.ByteString -> IO ()
putNextChunk inFile outFile chunkSize hash = do
  bdata <- B.hGet inFile chunkSize
  B.hPut outFile (B.append bdata hash)

mergeFile :: FilePath -> FilePath -> [B.ByteString] -> Int -> IO ()
mergeFile infile outfile hashes chunkSize = do
  inhandle <- openBinaryFile infile ReadMode
  outhandle <- openBinaryFile outfile WriteMode
  let rhashes = tail hashes ++ [B.empty]
  mapM_ (putNextChunk inhandle outhandle chunkSize) rhashes
  hClose outhandle
  hClose inhandle

encode :: FilePath -> FilePath -> Int -> IO B.ByteString
encode inFilePath outFilePath chunkSize = do
  inhandle <- openBinaryFile inFilePath ReadMode
  d <- loadDataBackToFront inhandle chunkSize
  hClose inhandle
  let hsh = reverse $ hashSequence d
  mergeFile inFilePath outFilePath hsh chunkSize
  return $ head hsh
