
module Blockchain.Data.Util (
  byteString2Integer,
  bytes2Integer,
  integer2Bytes,
  byteString2Int,
  bytes2Int,
  intg2Bytes
  ) where

import Data.Bits
import qualified Data.ByteString as B
import Data.Word

--I hate this, it is an ugly way to create an Integer from its component bytes.
--There should be an easier way....
--See http://stackoverflow.com/questions/25854311/efficient-packing-bytes-into-integers
byteString2Integer::B.ByteString->Integer
byteString2Integer x = bytes2Integer $ B.unpack x

bytes2Integer::[Word8]->Integer
bytes2Integer [] = 0
bytes2Integer (byte:rest) = fromIntegral byte `shift` (8 * length rest) + bytes2Integer rest


integer2Bytes::Integer->[Word8]
integer2Bytes 0 = []
integer2Bytes x = integer2Bytes (x `shiftR` 8) ++ [fromInteger (x .&. 255)]

{- Added so that addressStateNonce's and blockDataNumber's could be ints.
   this could all be done more DRYly if we replaced Integer with Integral and added type
   annotations everywhere. Done this way for now so as not to introduce annoying breaking changes.
   Functions below are not oveflow safe. -} 

intg2Bytes::Int->[Word8]
intg2Bytes 0 = []
intg2Bytes x = intg2Bytes (x `shiftR` 8) ++ [fromIntegral (x .&. 255)]

bytes2Int ::[Word8]->Int
bytes2Int [] = 0
bytes2Int (byte:rest) = fromIntegral byte `shift` (8 * length rest) + bytes2Int rest


byteString2Int::B.ByteString->Int
byteString2Int x = bytes2Int $ B.unpack x
