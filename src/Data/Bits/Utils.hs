{- arch-tag: Bit utilities main file
Copyright (c) 2004-2011 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file LICENSE
-}

{- |
   Module     : Data.Bits.Utils
   Copyright  : Copyright (C) 2004-2011 John Goerzen
   License    : BSD3

   Maintainer : John Goerzen <jgoerzen@complete.org> 
   Stability  : provisional
   Portability: portable to platforms with rawSystem

  Bit-related utilities

Written by John Goerzen, jgoerzen\@complete.org
-}

module Data.Bits.Utils(getBytes, fromBytes,
                     c2w8, s2w8, w82c, w82s)
where
import Data.Bits
import Data.Word

{- | Returns a list representing the bytes that comprise a data type.

Example:

> getBytes (0x12345678::Int) -> [0x12, 0x34, 0x56, 0x78]
-}
getBytes :: (Integral a, Bounded a, Bits a) => a -> [a]
getBytes input = 
    let getByte _ 0 = []
        getByte x remaining = (x .&. 0xff) : getByte (shiftR x 8) (remaining - 1)
        in
        if (bitSize input `mod` 8) /= 0
           then error "Input data bit size must be a multiple of 8"
           else reverse $ getByte input (bitSize input `div` 8)

{- | The opposite of 'getBytes', this function builds a number based on
its component bytes.

Results are undefined if any components of the input list are > 0xff!

-}

fromBytes :: (Bits a, Num a) => [a] -> a
fromBytes input =
    let dofb accum [] = accum
        dofb accum (x:xs) = dofb ((shiftL accum 8) .|. x) xs
        in
        dofb 0 input

{- | Converts a Char to a Word8. -}
c2w8 :: Char -> Word8
c2w8 = fromIntegral . fromEnum

{- | Converts a String to a [Word8]. -}
s2w8 :: String -> [Word8]
s2w8 = map c2w8

{- | Converts a Word8 to a Char. -}
w82c :: Word8 -> Char
w82c = toEnum . fromIntegral

{- | Converts a [Word8] to a String. -}
w82s :: [Word8] -> String
w82s = map w82c
