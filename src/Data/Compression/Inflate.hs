-- arch-tag: Inflate implementation for Haskell

{-
Inflate implementation for Haskell

Copyright 2004 Ian Lynagh <igloo@earth.li>
Licence: 3 clause BSD.

\section{Inflate}

This module provides a Haskell implementation of the inflate function,
as described by RFC 1951.

-}

{- |
   Module     : Data.Compression.Inflate
   Copyright  : Copyright (C) 2004 Ian Lynagh 
   License    : 3-clause BSD

   Maintainer : Ian Lynagh, 
   Maintainer : <igloo@earth.li>
   Stability  : provisional
   Portability: portable

Inflate algorithm implementation

Copyright (C) 2004 Ian Lynagh
-}

module Data.Compression.Inflate (inflate_string,
                                     inflate_string_remainder,
                                     inflate, Output, Bit,
                                    bits_to_word32) where

import Data.Array
import Data.List
import Data.Maybe
import qualified Data.Char
import Control.Monad

import Data.Bits
import Data.Word

inflate_string :: String -> String
inflate_string = fst . inflate_string_remainder
--    map (Data.Char.chr . fromIntegral) $ fst $ inflate $ map Data.Char.ord s

-- | Returns (Data, Remainder)
inflate_string_remainder :: String -> (String, String)
inflate_string_remainder s =
    let res = inflate $ map Data.Char.ord s
        convw32l l = map (Data.Char.chr . fromIntegral) l
        output = convw32l $ fst res
        b2w32 [] = []
        b2w32 b = let (this, next) = splitAt 8 b
                      in
                      bits_to_word32 this : b2w32 next
        remainder = convw32l $ b2w32 $ snd res
        in
        (output, remainder)

{-
\section{Types}

Type synonyms are your friend.

-}
type Output = [Word32] -- The final output

type Code = Word32     -- A generic code
type Dist = Code       -- A distance code
type LitLen = Code     -- A literal/length code
type Length = Word32   -- Number of bits needed to identify a code

type Table = InfM Code -- A Huffman table
type Tables = (Table, Table) -- lit/len and dist Huffman tables

{-

The \verb!Bit! datatype is used for the input. We can show values and
convert from the input we are given and to \verb!Word32!s which we us to
represent most values.

-}
newtype Bit = Bit Bool
    deriving Eq
instance Show Bit where
    show = (\x -> [x]) . show_b
    showList bs = showString $ "'" ++ map show_b bs ++ "'"

show_b :: Bit -> Char
show_b (Bit True) = '1'
show_b (Bit False) = '0'

int_to_bits :: Int -> [Bit]
int_to_bits = word8_to_bits . fromIntegral

word8_to_bits :: Word8 -> [Bit]
word8_to_bits n = map (\i -> Bit (testBit n i)) [0..7]

bits_to_word32 :: [Bit] -> Word32
bits_to_word32 = foldr (\(Bit b) i -> 2 * i + (if b then 1 else 0)) 0

{-

\section{Monad}

offset is rarely used, so make it strict to avoid building huge closures.

-}
data State = State { bits :: [Bit],                  -- remaining input bits
                     offset :: !Word32,              -- num bits consumed mod 8
                     history :: Array Word32 Word32, -- last 32768 output words
                     loc :: Word32                   -- where in history we are
                   }
data InfM a = InfM (State -> (a, State))

instance Monad InfM where
 -- (>>=)  :: InfM a -> (a -> InfM b) -> InfM b
    InfM v >>= f = InfM $ \s -> let (x, s') = v s
                                    InfM y = f x
                                in y s'
 -- return :: a -> InfM a
    return x = InfM $ \s -> (x, s)

set_bits :: [Bit] -> InfM ()
set_bits bs = InfM $ const ((), State bs 0 (array (0, 32767) []) 0)

{-
no_bits :: InfM Bool
no_bits = InfM $ \s -> (null (bits s), s)
-}

align_8_bits :: InfM ()
align_8_bits
 = InfM $ \s -> ((), s { bits = genericDrop ((8 - offset s) `mod` 8) (bits s),
                         offset = 0 })

get_bits :: Word32 -> InfM [Bit]
get_bits n = InfM $ \s -> case need n (bits s) of
                              (ys, zs) ->
                                  (ys, s { bits = zs,
                                           offset = (n + offset s) `mod` 8 } )
    where need 0 xs = ([], xs)
          need _ [] = error "get_bits: Don't have enough!"
          need i (x:xs) = let (ys, zs) = need (i-1) xs in (x:ys, zs)

extract_InfM :: InfM a -> (a, [Bit])
extract_InfM (InfM f) = let (x, s) = f undefined in (x, bits s)

output_w32 :: Word32 -> InfM ()
output_w32 w = InfM $ \s -> let l = loc s
                            in ((), s { history = history s // [(l, w)],
                                        loc = l + 1 })

repeat_w32s :: Word32 -> Word32 -> InfM [Word32]
repeat_w32s len dist
 = InfM $ \s -> let l = loc s
                    h = history s
                    new = map (h!) $ genericTake dist ([(l - dist) `mod` 32768..32767] ++ [0..])
                    new_bit = genericTake len (cycle new)
                    h' = h // zip (map (`mod` 32768) [l..]) new_bit
                in (new_bit, s { history = h', loc = (l + len) `mod` 32768 })

-----------------------------------

get_word32s :: Word32 -> Word32 -> InfM [Word32]
get_word32s _ 0 = return []
get_word32s b n = do w <- get_w32 b
                     ws <- get_word32s b (n-1)
                     return (w:ws)

get_w32 :: Word32 -> InfM Word32
get_w32 i = do bs <- get_bits i
               return (bits_to_word32 bs)

get_bit :: InfM Bit
get_bit = do [x] <- get_bits 1
             return x

{-
\section{Inflate itself}

The hardcore stuff!

-}
inflate :: [Int] -> (Output, [Bit])
inflate is = extract_InfM $ do set_bits $ concatMap int_to_bits is
                               x <- inflate_blocks False
                               align_8_bits
                               return x

-- Bool is true if we have seen the "last" block
inflate_blocks :: Bool -> InfM Output
inflate_blocks True = return []
inflate_blocks False
     = do [Bit is_last, Bit t1, Bit t2] <- get_bits 3
          case (t1, t2) of
              (False, False) ->
                  do align_8_bits
                     len <- get_w32 16
                     nlen <- get_w32 16
                     unless (len + nlen == 2^(32 :: Int) - 1)
                        $ error "inflate_blocks: Mismatched lengths"
                     ws <- get_word32s 8 len
                     mapM_ output_w32 ws
                     return ws
              (True, False) ->
                  inflate_codes is_last inflate_trees_fixed
              (False, True) ->
                  do tables <- inflate_tables
                     inflate_codes is_last tables
              (True, True) ->
                  error ("inflate_blocks: case 11 reserved")

inflate_tables :: InfM Tables
inflate_tables
 = do hlit <- get_w32 5
      hdist <- get_w32 5
      hclen <- get_w32 4
      llc_bs <- get_bits ((hclen + 4) * 3)
      let llc_bs' = zip (map bits_to_word32 $ triple llc_bs)
                        [16,17,18,0,8,7,9,6,10,5,11,4,12,3,13,2,14,1,15]
          tab = make_table llc_bs'
      lit_dist_lengths <- make_lit_dist_lengths tab
                                                (258 + hlit + hdist)
                                                (error "inflate_tables dummy")
      let (lit_lengths, dist_lengths) = genericSplitAt (257 + hlit)
                                                       lit_dist_lengths
          lit_table = make_table (zip lit_lengths [0..])
          dist_table = make_table (zip dist_lengths [0..])
      return (lit_table, dist_table)

triple :: [a] -> [[a]]
triple (a:b:c:xs) = [a,b,c]:triple xs
triple [] = []
triple _ = error "triple: can't happen"

make_lit_dist_lengths :: Table -> Word32 -> Word32 -> InfM [Word32]
make_lit_dist_lengths _ i _ | i < 0 = error "make_lit_dist_lengths i < 0"
make_lit_dist_lengths _ 0 _ = return []
make_lit_dist_lengths tab i last_thing
 = do c <- tab
      (ls, i', last_thing') <- meta_code i c last_thing
      ws <- make_lit_dist_lengths tab i' last_thing'
      return (ls ++ ws)

meta_code :: Word32 -> Code -> Word32 -> InfM ([Word32], Word32, Word32)
meta_code c i _ | i < 16 = return ([i], c - 1, i)
meta_code c 16 last_thing
                 = do xs <- get_bits 2
                      let l = 3 + bits_to_word32 xs
                      return (genericReplicate l last_thing, c - l, last_thing)
meta_code c 17 _ = do xs <- get_bits 3
                      let l = 3 + bits_to_word32 xs
                      return (genericReplicate l 0, c - l, 0)
meta_code c 18 _ = do xs <- get_bits 7
                      let l = 11 + bits_to_word32 xs
                      return (genericReplicate l 0, c - l, 0)
meta_code _ i _ = error $ "meta_code: " ++ show i

inflate_codes :: Bool -> Tables -> InfM Output
inflate_codes seen_last tabs@(tab_litlen, tab_dist)
 =
   {- do done <- no_bits
      if done
        then return [] -- XXX Is this right?
        else -}
             do i <- tab_litlen;
                if i == 256
                  then inflate_blocks seen_last
                  else
                       do pref <- if i < 256
                                  then do output_w32 i
                                          return [i]
                                  else case lookup i litlens of
                                           Nothing -> error "do_code_litlen"
                                           Just (base, num_bits) ->
                                               do extra <- get_w32 num_bits
                                                  let l = base + extra
                                                  dist <- dist_code tab_dist
                                                  repeat_w32s l dist
                          o <- inflate_codes seen_last tabs
                          return (pref ++ o)

litlens :: [(Code, (LitLen, Word32))]
litlens = zip [257..285] $ mk_bases 3 litlen_counts ++ [(258, 0)]
    where litlen_counts = [(8,0),(4,1),(4,2),(4,3),(4,4),(4,5)]

dist_code :: Table -> InfM Dist
dist_code tab
 = do code <- tab
      case lookup code dists of
          Nothing -> error "dist_code"
          Just (base, num_bits) -> do extra <- get_w32 num_bits
                                      return (base + extra)

dists :: [(Code, (Dist, Word32))]
dists = zip [0..29] $ mk_bases 1 dist_counts
    where dist_counts = (4,0):map ((,) 2) [1..13]

mk_bases :: Word32 -> [(Int, Word32)] -> [(Word32, Word32)]
mk_bases base counts = snd $ mapAccumL next_base base incs
            where next_base current bs = (current + 2^bs, (current, bs))
                  incs = concat $ map (uncurry replicate) counts

{-
\section{Fixed tables}

The fixed tables. Not much to say really.

-}
inflate_trees_fixed :: Tables
inflate_trees_fixed = (make_table $ [(8, c) | c <- [0..143]]
                                 ++ [(9, c) | c <- [144..255]]
                                 ++ [(7, c) | c <- [256..279]]
                                 ++ [(8, c) | c <- [280..287]],
                       make_table [(5, c) | c <- [0..29]])

{-
\section{The Huffman Tree}

As the name suggests, the obvious way to store Huffman trees is in a
tree datastructure. Externally we want to view them as functions though,
so we wrap the tree with \verb!get_code! which takes a list of bits and
returns the corresponding code and the remaining bits. To make a tree
from a list of length code pairs is a simple recursive process.

-}
data Tree = Branch Tree Tree | Leaf Word32 | Null

make_table :: [(Length, Code)] -> Table
make_table lcs = case make_tree 0 $ sort $ filter ((/= 0) . fst) lcs of
                     (tree, []) -> get_code tree
                     _ -> error $ "make_table: Left-over lcs from"

get_code :: Tree -> InfM Code
get_code (Branch zero_tree one_tree)
 = do Bit b <- get_bit
      if b then get_code one_tree else get_code zero_tree
get_code (Leaf w) = return w
get_code Null = error "get_code Null"

make_tree :: Word32 -> [(Length, Code)] -> (Tree, [(Length, Code)])
make_tree _ [] = (Null, [])
make_tree i lcs@((l, c):lcs')
 | i == l = (Leaf c, lcs')
 | i < l = let (zero_tree, lcs_z) = make_tree (i+1) lcs
               (one_tree, lcs_o) = make_tree (i+1) lcs_z
           in (Branch zero_tree one_tree, lcs_o)
 | otherwise = error "make_tree: can't happen"


