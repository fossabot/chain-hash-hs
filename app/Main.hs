{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards    #-}

module Main where

import           Crypto.ChainHash
import           System.Console.CmdArgs
import           System.Environment     (getArgs, withArgs)

{-# ANN block_size ("HLint: ignore Use camelCase" :: String) #-}
data ChainHash
  = Encode { input      :: Maybe FilePath
           , output     :: Maybe FilePath
           , block_size :: Int }
  | Decode { input      :: Maybe FilePath
           , output     :: Maybe FilePath
           , checksum   :: Maybe String
           , block_size :: Int }
  deriving (Show, Data, Typeable)

encodeCmd :: ChainHash
encodeCmd =
  Encode
  { input = def &= help "file to encode"
  , output = def &= help "path to write encoded file to"
  , block_size = 1024 &= help "number of bytes in a block"
  } &=
  help "Encode a file using the chain-hash"

decodeCmd :: ChainHash
decodeCmd =
  Decode
  { input = def &= help "file to encode"
  , output = def &= help "path to write encoded file to"
  , checksum = def &= help "initial checksum for the first block"
  , block_size = 1024 &= help "number of bytes in a block"
  } &=
  help "Decode a file that was encoded with the chain-hash"

defaultFile :: FilePath
defaultFile = "resources/example.txt"

validateAndRun :: ChainHash -> IO (Either String ())
validateAndRun Encode { input = Just input
                      , output = Just output
                      , block_size = block_size
                      } = do
  hash <- encode input output block_size
  putStr $ hexify hash ++ "\n"
  return $ Right ()
validateAndRun Encode {input = Nothing, ..} = return $ Left "Missing --input"
validateAndRun Encode {output = Nothing, ..} = return $ Left "Missing --output"
validateAndRun Decode { input = Just input
                      , output = Just output
                      , checksum = Just checksum
                      , block_size = block_size
                      } = do
  decode input output (unhexify checksum) block_size
  return $ Right ()
validateAndRun Decode {input = Nothing, ..} = return $ Left "Missing --input"
validateAndRun Decode {output = Nothing, ..} = return $ Left "Missing --output"
validateAndRun Decode {checksum = Nothing, ..} =
  return $ Left "Missing --checksum"

mode :: Mode (CmdArgs ChainHash)
mode =
  cmdArgsMode $
  modes [encodeCmd, decodeCmd] &=
  help
    "Encode and decode arbitrarily large files with a chain-hashing function." &=
  program "chain-hash-hs-exe" &=
  summary "ChainHash v0.1.0"

handleErrorState :: String -> IO ()
handleErrorState errorMsg = putStr $ "\n" ++ errorMsg ++ "\n"

main :: IO ()
main = do
  suppliedArgs <- getArgs
  opts <-
    (if null suppliedArgs
       then withArgs ["--help"]
       else id) $
    cmdArgsRun mode
  result <- validateAndRun opts
  either handleErrorState return result
