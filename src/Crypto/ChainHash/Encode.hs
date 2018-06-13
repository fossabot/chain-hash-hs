module Crypto.ChainHash.Encode (encode) where

import System.IO
import qualified Data.ByteString as B
import qualified Crypto.Hash.SHA256 as Sha256

readData :: Handle -> Integer -> Integer -> IO B.ByteString
readData handle start size = do
    hSeek handle AbsoluteSeek start
    B.hGet handle (fromInteger size)

calculateChunks :: Handle -> Integer -> IO [(Integer, Integer)]
calculateChunks handle chunkSize = do
    totalSize <- hFileSize handle
    return $ map (\n -> (n, min chunkSize (totalSize - n))) [0,chunkSize..totalSize]

loadDataBackToFront :: Handle -> Integer -> IO [B.ByteString]
loadDataBackToFront handle chunkSize = do
    positions <- calculateChunks handle chunkSize
    mapM (uncurry (readData handle)) (reverse positions)


hashScan :: B.ByteString -> B.ByteString -> B.ByteString
hashScan prev curr = Sha256.hash (B.append curr prev)

hashSequence :: [B.ByteString] -> [B.ByteString]
hashSequence bsData = scanl hashScan (Sha256.hash (head bsData)) (tail bsData)


mergeFile :: FilePath -> FilePath -> [B.ByteString] -> Integer -> IO ()
mergeFile infile outfile hashes chunkSize = do
    inhandle <- openBinaryFile infile ReadMode
    outhandle <- openBinaryFile outfile WriteMode
    let rhashes = tail hashes ++ [B.empty]
    mapM_ (\ x -> do
        bdata <- B.hGet inhandle (fromInteger chunkSize)
        B.hPut outhandle (B.append bdata x)) rhashes
    hClose outhandle
    hClose inhandle

encode :: FilePath -> FilePath -> Integer -> IO B.ByteString
encode inFilePath outFilePath chunkSize = do
    inhandle <- openBinaryFile inFilePath ReadMode
    d <- loadDataBackToFront inhandle chunkSize
    hClose inhandle
    let hsh = reverse $ hashSequence d
    mergeFile inFilePath outFilePath hsh chunkSize
    return $ head hsh
