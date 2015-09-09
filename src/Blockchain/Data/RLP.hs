{-# LANGUAGE FlexibleInstances #-}

-- | The RLP module provides a framework within which serializers can be built, described in the Ethereum Yellowpaper (<http://gavwood.com/paper.pdf>).
--
-- The 'RLPObject' is an intermediate data container, whose serialization rules are well defined.  By creating code that converts from a
-- given type to an 'RLPObject', full serialization will be specified.  The 'RLPSerializable' class provides functions to do this conversion.

module Blockchain.Data.RLP (
  RLPObject(..),
  formatRLPObject,
  RLPSerializable(..),
  rlpSplit,
  rlpSerialize,
  rlpDeserialize
  ) where

import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BC
import Data.ByteString.Internal
import Data.Functor
import Data.Word
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
import Numeric

import Blockchain.Data.Util

-- | An internal representation of generic data, with no type information.
--
-- End users will not need to directly create objects of this type (an 'RLPObject' can be created using 'rlpEncode'),
-- however the designer of a new type will need to create conversion code by making their type an instance 
-- of the RLPSerializable class. 
data RLPObject = RLPScalar Word8 | RLPString B.ByteString | RLPArray [RLPObject] deriving (Show, Eq, Ord)

-- | Converts objects to and from 'RLPObject's.
class RLPSerializable a where
  rlpDecode::RLPObject->a
  rlpEncode::a->RLPObject


instance Pretty RLPObject where
  pretty (RLPArray objects) =
    encloseSep (text "[") (text "]") (text ", ") $ pretty <$> objects
  pretty (RLPScalar n) = text $ "0x" ++ showHex n ""
  pretty (RLPString s) = text $ "0x" ++ BC.unpack (B16.encode s)

formatRLPObject::RLPObject->String
formatRLPObject = show . pretty
                         
splitAtWithError::Int->[a]->([a], [a])
splitAtWithError 0 rest = ([], rest)
splitAtWithError _ [] = error "splitAtWithError called with n > length arr"
splitAtWithError n (first:rest) = (first:val, rest')
  where (val, rest') = splitAt (n-1) rest

getLength::Int->[Word8]->(Integer, [Word8])
getLength sizeOfLength bytes = (bytes2Integer $ take sizeOfLength bytes, drop sizeOfLength bytes)

rlpSplit::[Word8]->(RLPObject, [Word8])
rlpSplit (x:rest) | x >= 192 && x <= 192+55 =
  (RLPArray $ getRLPObjects arrayData, nextRest)
  where
    dataLength::Word8
    dataLength = x - 192
    (arrayData, nextRest) = splitAtWithError (fromIntegral dataLength) rest
rlpSplit (x:rest) | x >= 0xF8 && x <= 0xFF =
  (RLPArray $ getRLPObjects arrayData, nextRest)
  where
    (arrLength, restAfterLen) = getLength (fromIntegral x - 0xF7) rest
    (arrayData, nextRest) = splitAtWithError (fromIntegral arrLength) restAfterLen
rlpSplit (249:len1:len2:rest) =
  (RLPArray $ getRLPObjects arrayData, nextRest)
  where
    len = fromIntegral len1 `shift` 8 + fromIntegral len2
    (arrayData, nextRest) = splitAtWithError len rest
rlpSplit (x:rest) | x >= 128 && x <= 128+55 =
  (RLPString $ B.pack strList, nextRest)
  where
    strLength = x-128
    (strList, nextRest) = splitAtWithError (fromIntegral strLength) rest
rlpSplit (x:rest) | x >= 0xB8 && x <= 0xBF =
  (RLPString $ B.pack strList, nextRest)
  where
    (strLength, restAfterLen) = getLength (fromIntegral x - 0xB7) rest
    (strList, nextRest) = splitAtWithError (fromIntegral strLength) restAfterLen
rlpSplit (x:rest) | x < 128 =
  (RLPScalar x, rest)
rlpSplit x = error ("Missing case in rlpSplit: " ++ show (B.pack x))

getRLPObjects::[Word8]->[RLPObject]
getRLPObjects [] = []
getRLPObjects theData = obj:getRLPObjects rest
  where
    (obj, rest) = rlpSplit theData

int2Bytes::Int->[Word8]
int2Bytes val | val < 0x100 = map (fromIntegral . (val `shiftR`)) [0]
int2Bytes val | val < 0x10000 = map (fromIntegral . (val `shiftR`)) [8, 0]
int2Bytes val | val < 0x1000000 = map (fromIntegral . (val `shiftR`)) [16,  8, 0]
int2Bytes val | val < 0x100000000 = map (fromIntegral . (val `shiftR`)) [24, 16..0]
int2Bytes val | val < 0x10000000000 = map (fromIntegral . (val `shiftR`)) [32, 24..0]
int2Bytes _ = error "int2Bytes not defined for val >= 0x10000000000."

rlp2Bytes::RLPObject->[Word8]
rlp2Bytes (RLPScalar val) = [fromIntegral val]
rlp2Bytes (RLPString s) | B.length s <= 55 = 0x80 + fromIntegral (B.length s):B.unpack s
rlp2Bytes (RLPString s) =
  [0xB7 + fromIntegral (length lengthAsBytes)] ++ lengthAsBytes ++ B.unpack s
  where
    lengthAsBytes = int2Bytes $ B.length s
rlp2Bytes (RLPArray innerObjects) =
  if length innerBytes <= 55
  then 0xC0 + fromIntegral (length innerBytes):innerBytes
  else let lenBytes = int2Bytes $ length innerBytes
       in [0xF7 + fromIntegral (length lenBytes)] ++ lenBytes ++ innerBytes
  where
    innerBytes = concat $ rlp2Bytes <$> innerObjects

--TODO- Probably should just use Data.Binary's 'Binary' class for this

-- | Converts bytes to 'RLPObject's.
--
-- Full deserialization of an object can be obtained using @rlpDecode . rlpDeserialize@.
rlpDeserialize::B.ByteString->RLPObject
rlpDeserialize s = 
  case rlpSplit $ B.unpack s of
    (o, []) -> o
    _ -> error ("parse error converting ByteString to an RLP Object: " ++ show (B.unpack s))


-- | Converts 'RLPObject's to bytes.
--
-- Full serialization of an object can be obtained using @rlpSerialize . rlpEncode@.
rlpSerialize::RLPObject->B.ByteString
rlpSerialize o = B.pack $ rlp2Bytes o


instance RLPSerializable Integer where
  rlpEncode 0 = RLPString B.empty
  rlpEncode x | x < 128 = RLPScalar $ fromIntegral x
  rlpEncode x = RLPString $ B.pack $ integer2Bytes x
  rlpDecode (RLPScalar x) = fromIntegral x
  rlpDecode (RLPString s) = byteString2Integer s
  rlpDecode (RLPArray _) = error "rlpDecode called for Integer for array"

instance RLPSerializable String where
  rlpEncode s = rlpEncode $ BC.pack s

  rlpDecode (RLPString s) = BC.unpack s
  rlpDecode (RLPScalar n) = [w2c $ fromIntegral n]
  rlpDecode (RLPArray x) = error $ "Malformed RLP in call to rlpDecode for String: RLPObject is an array: " ++ show (pretty x)

instance RLPSerializable B.ByteString where
    rlpEncode x | B.length x == 1 && B.head x < 128 = RLPScalar $ B.head x
    rlpEncode s = RLPString s
      
    rlpDecode (RLPScalar x) = B.singleton x
    rlpDecode (RLPString s) = s
    rlpDecode x = error ("rlpDecode for ByteString not defined for: " ++ show x)
