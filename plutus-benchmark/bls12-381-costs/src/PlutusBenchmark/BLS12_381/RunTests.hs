-- editorconfig-checker-disable-file
{- | Print out the costs of various test scripts involving the BLS12_381
   primitives.  Most of these work on varying numbers of inputs so that we can
   get an idea of what we can do within the on-chain execution limits.
-}
module PlutusBenchmark.BLS12_381.RunTests (runTests)
where

import PlutusBenchmark.BLS12_381.Scripts
import PlutusBenchmark.Common

import PlutusTx.Prelude as Tx hiding (sort, (*))

import Data.ByteString qualified as BS
import System.IO (Handle)
import Text.Printf (hPrintf)

import Prelude (IO, mapM_)

-- Run various tests and print information about their costs

printCosts_HashAndAddG1 :: Handle -> Integer -> IO ()
printCosts_HashAndAddG1 h n =
    let script = mkHashAndAddG1Script (listOfSizedByteStrings n 4)
    in printSizeStatistics h (TestSize n) script

printCosts_HashAndAddG2 :: Handle -> Integer -> IO ()
printCosts_HashAndAddG2 h n =
    let script = mkHashAndAddG2Script (listOfSizedByteStrings n 4)
    in printSizeStatistics h (TestSize n) script

printCosts_UncompressAndAddG1 :: Handle -> Integer -> IO ()
printCosts_UncompressAndAddG1 h n =
    let script = mkUncompressAndAddG1Script (listOfSizedByteStrings n 4)
    in printSizeStatistics h (TestSize n) script

printCosts_UncompressAndAddG2 :: Handle -> Integer -> IO ()
printCosts_UncompressAndAddG2 h n =
    let script = mkUncompressAndAddG2Script (listOfSizedByteStrings n 4)
    in printSizeStatistics h (TestSize n) script

printCosts_Pairing :: Handle -> IO ()
printCosts_Pairing h = do
    let emptyDST = toBuiltin BS.empty
        p1 = Tx.bls12_381_G1_hashToGroup (toBuiltin . BS.pack $ [0x23, 0x43, 0x56, 0xf2]) emptyDST
        p2 = Tx.bls12_381_G2_hashToGroup (toBuiltin . BS.pack $ [0x10, 0x00, 0xff, 0x88]) emptyDST
        q1 = Tx.bls12_381_G1_hashToGroup (toBuiltin . BS.pack $ [0x11, 0x22, 0x33, 0x44]) emptyDST
        q2 = Tx.bls12_381_G2_hashToGroup (toBuiltin . BS.pack $ [0xa0, 0xb1, 0xc2, 0xd3]) emptyDST
        script = mkPairingScript p1 p2 q1 q2
    printSizeStatistics h NoSize script

printCosts_Groth16Verify :: Handle -> IO ()
printCosts_Groth16Verify h = do
  let script = mkGroth16VerifyScript
  printSizeStatistics h NoSize script

runTests :: Handle -> IO ()
runTests h = do

  hPrintf h "Hash n bytestrings onto G1 and add points\n\n"
  printHeader h
  mapM_ (printCosts_HashAndAddG1 h) [0, 10..150]
  hPrintf h "\n\n"

  hPrintf h "Hash n bytestrings onto G2 and add points\n\n"
  printHeader h
  mapM_ (printCosts_HashAndAddG2 h) [0, 10..150]
  hPrintf h "\n\n"

  hPrintf h "Uncompress n G1 points and add the results\n\n"
  printHeader h
  mapM_ (printCosts_UncompressAndAddG1 h) [0, 10..150]
  hPrintf h "\n\n"

  hPrintf h "Uncompress n G2 points and add the results\n\n"
  printHeader h
  mapM_ (printCosts_UncompressAndAddG2 h) [0, 10..150]
  hPrintf h "\n\n"

  hPrintf h "Apply pairing to two pairs of points in G1 x G2 and run finalVerify on the results\n\n"
  printHeader h
  printCosts_Pairing h
  hPrintf h "\n\n"

  hPrintf h "Groth16 verification example\n\n"
  printHeader h
  printCosts_Groth16Verify h
  hPrintf h "\n"

  if checkGroth16Verify_Haskell
  then hPrintf h "Groth16Verify succeeded\n"
  else hPrintf h "Groth16Verify failed\n"
