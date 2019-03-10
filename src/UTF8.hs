-- Internal module to support UTF8
module UTF8 (toUTF8BS) where

import Data.Char (ord)
import Data.Bits
import Data.Word (Word8)
import qualified Data.ByteString as BS

toUTF8BS :: String -> BS.ByteString
toUTF8BS = BS.pack . encodeStringUtf8

-- | Encode 'String' to a list of UTF8-encoded octets
--
-- Code-points in the @U+D800@-@U+DFFF@ range will be encoded
-- as the replacement character (i.e. @U+FFFD@).
--
-- The code is extracted from Cabal library, written originally HVR
encodeStringUtf8 :: String -> [Word8]
encodeStringUtf8 []        = []
encodeStringUtf8 (c:cs)
  | c <= '\x07F' = w8
                 : encodeStringUtf8 cs
  | c <= '\x7FF' = (0xC0 .|.  w8ShiftR  6          )
                 : (0x80 .|. (w8          .&. 0x3F))
                 : encodeStringUtf8 cs
  | c <= '\xD7FF'= (0xE0 .|.  w8ShiftR 12          )
                 : (0x80 .|. (w8ShiftR  6 .&. 0x3F))
                 : (0x80 .|. (w8          .&. 0x3F))
                 : encodeStringUtf8 cs
  | c <= '\xDFFF'= 0xEF : 0xBF : 0xBD -- U+FFFD
                 : encodeStringUtf8 cs
  | c <= '\xFFFF'= (0xE0 .|.  w8ShiftR 12          )
                 : (0x80 .|. (w8ShiftR  6 .&. 0x3F))
                 : (0x80 .|. (w8          .&. 0x3F))
                 : encodeStringUtf8 cs
  | otherwise    = (0xf0 .|.  w8ShiftR 18          )
                 : (0x80 .|. (w8ShiftR 12 .&. 0x3F))
                 : (0x80 .|. (w8ShiftR  6 .&. 0x3F))
                 : (0x80 .|. (w8          .&. 0x3F))
                 : encodeStringUtf8 cs
  where
    w8 = fromIntegral (ord c) :: Word8
    w8ShiftR :: Int -> Word8
    w8ShiftR = fromIntegral . shiftR (ord c)
