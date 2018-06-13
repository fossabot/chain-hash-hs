{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards    #-}

module Main where

import           Crypto.ChainHash
import           System.Console.CmdArgs

data ChainHash
  = Encode { input     :: Maybe FilePath
           , output    :: Maybe FilePath
           , blockSize :: Integer }
  | Decode { input     :: Maybe FilePath
           , output    :: Maybe FilePath
           , checksum  :: Maybe String
           , blockSize :: Integer }
  deriving (Show, Data, Typeable)

encodeCmd =
  Encode
  { input = def &= help "file to encode"
  , output = def &= help "path to write encoded file to"
  , blockSize = 1024 &= help "number of bytes in a block"
  }

decodeCmd =
  Decode
  { input = def &= help "file to encode"
  , output = def &= help "path to write encoded file to"
  , checksum = def &= help "initial checksum for the first block"
  , blockSize = 1024 &= help "number of bytes in a block"
  }

defaultFile :: FilePath
defaultFile = "resources/example.txt"

validateAndRun :: ChainHash -> IO (Either String ())
validateAndRun Encode { input = Just input
                      , output = Just output
                      , blockSize = blockSize
                      } = do
                        hash <- encode input output blockSize
                        putStr $ hexify hash ++ "\n"
                        return $ Right ()
validateAndRun Encode {input = Nothing,..} = return $ Left "Missing input filename."
validateAndRun Encode {output = Nothing,..} = return $ Left "Missing output filename."
validateAndRun Decode { input = Just input
                      , output = Just output
                      , checksum = Just checksum
                      , blockSize = blockSize
                      } = do
                        decode input output (unhexify checksum) blockSize
                        return $ Right ()
validateAndRun Decode {input = Nothing,..} = return $ Left "Missing input filename."
validateAndRun Decode {output = Nothing,..} = return $ Left "Missing output filename."
validateAndRun Decode {checksum = Nothing,..} = return $ Left "Missing initial checksum."

main :: IO ()
main = do
  mode <- cmdArgs (modes [encodeCmd, decodeCmd])
  result <- validateAndRun mode
  case result of
    Left errorMsg -> do
        putStr $ "\n"++ errorMsg ++ "\n"
        print $ cmdArgsMode mode
    Right () -> return ()
--  ihash <- encode defaultFile (defaultFile ++ ".encoded") 1024
--  print (hexify ihash)
--  decode (defaultFile ++ ".encoded") (defaultFile ++ ".decoded") ihash 1024
