-- SHA1 hash function.

-- SHA1:
-- http://sea-to-sky.net/~sreid/sha1.c

module SHA1 where

import Int
import Ptr
import MarshalAlloc
import CString
import CTypes
import IOExts
import Storable (pokeByteOff)
import Bits
import Char

type SHA1_CTX = ()
type SHA1_DIGEST = Ptr CChar

foreign import ccall "sha1lib.h SHA1Init"
	sha1_init :: Ptr SHA1_CTX -> IO ()
foreign import ccall "sha1lib.h SHA1Update"
	sha1_update :: Ptr SHA1_CTX -> Ptr CChar -> Int32 -> IO ()
foreign import ccall "sha1lib.h SHA1Final"
	sha1_final :: SHA1_DIGEST -> Ptr SHA1_CTX -> IO ()

sha1 :: String -> String
sha1 str =
	unsafePerformIO $
	do	sha1_context <- mallocBytes (64 + 5*4 + 2*4)
		sha1_digest <- mallocBytes 20
		sha1_init sha1_context
		let loop s =
			-- Process 16KB block in every round.
			case splitAt 16384 s of
				(xs, ys) ->
					do	cs <- newCString xs
						sha1_update sha1_context cs (fromIntegral (length xs))
						case ys of
							[] -> return ()
							_ -> loop ys
		loop str
		sha1_final sha1_digest sha1_context
		peekCStringLen (sha1_digest, 20)														


	


