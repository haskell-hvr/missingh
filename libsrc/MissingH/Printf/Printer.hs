{- arch-tag: Printf printer declarations
Copyright (C) 2003 Ian Lynagh
Released under the GNU LGPL 2.1
See the COPYRIGHT and 3rd-party-licenses/LGPL-2.1 files for more details
-}

{- |
   Module     : MissingH.Printf.Types
   Copyright  : Copyright (C) 2003 Ian Lynagh
   License    : GNU GPL, version 2 or above OR GNU LGPL 2.1

   Maintainer : John Goerzen, 
   Maintainer : jgoerzen@complete.org
   Stability  : provisional
   Portability: portable

This module is used internally by "MissingH.Printf" and is /not intended
to be used in your programs/.

Copyright (c) 2003 Ian Lynagh
-}

{- Modified November 2004 by John Goerzen
 * Extracted from Printf 0.1.0 Printer.lhs and Parser.lhs
 * Converted from lhs to hs
 * Converted to work without TH and with my Printf typing system
 * Converted to work with MissingH module names
-}

module MissingH.Printf.Printer (get_conversion_func, thousandify, octalify, hexify, fix_width
) where

import Maybe (fromMaybe)
import Numeric (showEFloat, showFFloat)
import Char (toLower, toUpper)
import MissingH.Printf.Types
import Data.List(genericLength, genericReplicate, genericTake)

{-
xn where n is an integer refers to an argument to the function
y*, n* is reserved for %n
fw* is reserved for field width intermediates
Everything else can be used by the conversion functions
-}

get_conversion_func :: Char -> ConversionFunc
get_conversion_func c = fromMaybe (error (c:": CF unknown")) $ lookup c cfs
    where cfs = [
                 ('d', print_signed_int),
                 ('i', print_signed_int),
                 ('o', print_unsigned_int 'o'),
                 ('u', print_unsigned_int 'u'),
                 ('x', print_unsigned_int 'x'),
                 ('X', print_unsigned_int 'X'),
                 ('e', print_exponent_double 'e'),
                 ('E', print_exponent_double 'E'),
                 ('f', print_fixed_double 'f'),
                 ('F', print_fixed_double 'F'),
                 -- 'g' not handled
                 -- 'G' not handled
                 -- 'a' not handled
                 -- 'A' not handled
                 ('c', print_char),
                 ('s', print_string),
                 ('C', print_char),
                 ('S', print_string),
                 -- 'p' makes no sense
                 -- 'n' handled elsewhere
                 -- '%' handled elsewhere
                 ('H', show_arg) -- Haskell extension
                ]

-- %d, %i
print_signed_int :: ConversionFunc
print_signed_int argv flags mw mp = res
    where arg = (fromValue argv)::Integer
          preci = fromMaybe 1 mp
          width = fromMaybe 0 mw
          disp | Thousands `elem` flags = thousandify . show
               | otherwise              =               show
          plus_sign = if Plus `elem` flags
                      then "+"
                      else if BlankPlus `elem` flags
                      then " "
                      else ""
          res =    let to_show = toInteger arg
                       shown = disp $ abs to_show
                       w = ( if ZeroPadded `elem` flags
                              then (preci `max` width - genericLength sign)::Width
                              else (preci)::Width )
                       sign = if to_show < 0 then "-" else plus_sign
                       num_zeroes = (w - genericLength shown) `max` 0
                   in sign ++ genericReplicate num_zeroes '0' ++ shown
                 

-- %o, u, x, X
print_unsigned_int :: Char -> ConversionFunc
print_unsigned_int base argv flags mw mp = res
    where arg = (fromValue argv)::Integer
          preci = fromMaybe 1  mp
          width = fromMaybe 0 mw
          w = if ZeroPadded `elem` flags then (preci) `max` width
                                         else  preci
          disp = case base of
                     'o' -> octalify
                     'x' -> hexify ({-lift-} lower_hex)
                     'X' -> hexify ({-lift-} upper_hex)
                     'u' | Thousands `elem` flags -> thousandify . show
                         | otherwise              ->                show
                     _ -> err_letter
          prefix = if AlternateForm `elem` flags then case base of
                                                          'o' -> "0"
                                                          'u' -> ""
                                                          'x' -> "0x"
                                                          'X' -> "0X"
                                                          _ -> err_letter
                                                 else ""
          res =    let to_show = toInteger arg `max` 0
                       shown = disp to_show
                       pref = if to_show == 0 then "" else prefix
                       num_zeroes = (w - genericLength shown - genericLength pref) `max` 0
                   in pref ++ genericReplicate num_zeroes '0' ++ shown
                 
          err_letter = error "print_unsigned_int: Bad letter"

-- %e, E
print_exponent_double :: Char -> ConversionFunc
print_exponent_double e argv flags mw mp = res
    where arg = (fromValue argv)::Double
          preci = fromMaybe 6 mp
          width = fromMaybe 0 mw
          plus_sign = if Plus `elem` flags
                      then "+"
                      else if BlankPlus `elem` flags
                      then " "
                      else ""
          keep_dot = AlternateForm `elem` flags
          res =    let to_show = (fromRational $ toRational arg) :: Double
                       shown = showEFloat (Just (fromInteger preci)) (abs to_show) ""
                       sign = if to_show < 0 then "-" else plus_sign
                       fix_prec0 = if (preci) == 0
                                   then case break (== '.') shown of
                                            (xs, _:_:ys)
                                                | keep_dot  -> xs ++ '.':ys
                                                | otherwise -> xs ++ ys
                                            _ -> shown
                                   else shown
                       fix_exp_sign = case break (== 'e') fix_prec0 of
                                          (xs, 'e':'-':ys) -> xs ++ 'e':'-':ys
                                          (xs, 'e':ys)     -> xs ++ 'e':'+':ys
                       fix_exp = case break (== 'e') fix_exp_sign of
                                     (xs, [_,s,y]) -> xs ++ [e,s,'0',y]
                                     (xs, _:ys) -> xs ++ e:ys
                       num_zeroes = (width - genericLength fix_exp - genericLength sign)
                              `max` 0
                   in sign ++ genericReplicate num_zeroes '0' ++ fix_exp
                 

-- %f, F
print_fixed_double :: Char -> ConversionFunc
print_fixed_double f argv flags mw mp = res
    where arg = (fromValue argv)::Double
          preci = fromMaybe 6  mp
          width = fromMaybe 0  mw
          plus_sign = if Plus `elem` flags
                      then "+"
                      else if BlankPlus `elem` flags
                      then " "
                      else ""
          add_dot = AlternateForm `elem` flags
          fix_case | f == 'f'  = map toLower
                   | otherwise = map toUpper
          res =    let to_show = (fromRational $ toRational arg) :: Double
                       shown = showFFloat (Just (fromInteger preci)) (abs to_show) ""
                       shown' = if add_dot && (preci) == 0 then shown ++ "."
                                                          else shown
                       sign = if to_show < 0 then "-" else plus_sign
                       num_zeroes = (width - genericLength shown' - genericLength sign)
                              `max` 0
                   in sign ++ genericReplicate num_zeroes '0' ++ fix_case shown'
                 

-- %c, C
print_char :: ConversionFunc
print_char arg _ _ _ = [(fromValue arg)::Char]

-- %s, S
print_string :: ConversionFunc
print_string argv _ _ mp
    = case mp of
          Just preci -> if preci < 0 then arg else genericTake preci arg
          Nothing -> arg
      where arg = fromValue argv

-- Corresponds to %H (Haskell extension)
show_arg :: ConversionFunc
show_arg argv flags mw mp = (print_string (toValue (showValue argv))) flags mw mp

lower_hex, upper_hex :: Bool
lower_hex = False
upper_hex = True

hexify :: Bool -> Integer -> String
hexify _ 0 = "0"
hexify upper i = to_base 16 ((digits !!) . fromInteger) i
    where digits | upper     = ['0'..'9'] ++ ['A'..'F']
                 | otherwise = ['0'..'9'] ++ ['a'..'f']

octalify :: Integer -> String
octalify 0 = "0"
octalify i = to_base 8 ((['0'..'7'] !!) . fromInteger) i

to_base :: Integer           -- Base
        -> (Integer -> Char) -- Digit maker
        -> Integer           -- Number to convert, > 0
        -> String
to_base = f ""
    where f s _    _       0 = s
          f s base mkDigit i = case i `divMod` base of
                                   (i', d) -> f (mkDigit d:s) base mkDigit i'

thousandify :: String -> String
thousandify = reverse . t . reverse
    where t (x1:x2:x3:xs@(_:_)) = x1:x2:x3:',':t xs
          t xs = xs


----------------------------------------------------------------------
-- FROM Ian Lynagh's Parser.lhs
----------------------------------------------------------------------


fix_width :: [Flag] -> Maybe Width -> String -> String
fix_width _ Nothing e = e
fix_width flags (Just w) e = exp_spaced
    where
          exp_num_spaces = abs w - genericLength e
          exp_num_spaces' = 0 `max` exp_num_spaces
          exp_spaces = genericReplicate exp_num_spaces' ' '
          exp_left_padded = e ++ exp_spaces
          exp_right_padded = exp_spaces ++ e
          exp_spaced = if LeftAdjust `elem` flags
                       then exp_left_padded
                       else if w < 0 then exp_left_padded
                                      else exp_right_padded


