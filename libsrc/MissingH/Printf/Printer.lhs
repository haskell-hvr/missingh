arch-tag: Printf printer declarations

\begin{code}
module Printer (get_conversion_func, thousandify, octalify, hexify) where

import Language.Haskell.THSyntax
import Maybe (fromMaybe)
import Numeric (showEFloat, showFFloat)
import Char (toLower, toUpper)
import Types

{-
xn where n is an integer refers to an argument to the function
y*, n* is reserved for %n
fw* is reserved for field width intermediates
Everything else can be used by the conversion functions
-}

type ConversionFunc = Arg
                   -> [Flag]
                   -> Maybe Width
                   -> Maybe Precision
                   -> ExpQ

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
print_signed_int arg flags mw mp = res
    where preci = fromMaybe [| 1 |] mp
          width = fromMaybe [| 0 |] mw
          disp | Thousands `elem` flags = [| thousandify . show |]
               | otherwise              = [|               show |]
          plus_sign = if Plus `elem` flags
                      then "+"
                      else if BlankPlus `elem` flags
                      then " "
                      else ""
          res = [| let to_show = toInteger $arg
                       shown = $disp $ abs to_show
                       w = $( if ZeroPadded `elem` flags
                              then [| $preci `max` $width - length sign |]
                              else preci )
                       sign = if to_show < 0 then "-" else plus_sign
                       num_zeroes = (w - length shown) `max` 0
                   in sign ++ replicate num_zeroes '0' ++ shown
                 |]

-- %o, u, x, X
print_unsigned_int :: Char -> ConversionFunc
print_unsigned_int base arg flags mw mp = res
    where preci = fromMaybe [| 1 |] mp
          width = fromMaybe [| 0 |] mw
          w = if ZeroPadded `elem` flags then [| $preci `max` $width |]
                                         else     preci
          disp = case base of
                     'o' -> [| octalify |]
                     'x' -> [| hexify $(lift lower_hex) |]
                     'X' -> [| hexify $(lift upper_hex) |]
                     'u' | Thousands `elem` flags -> [| thousandify . show |]
                         | otherwise              -> [|               show |]
                     _ -> err_letter
          prefix = if AlternateForm `elem` flags then case base of
                                                          'o' -> "0"
                                                          'u' -> ""
                                                          'x' -> "0x"
                                                          'X' -> "0X"
                                                          _ -> err_letter
                                                 else ""
          res = [| let to_show = toInteger $arg `max` 0
                       shown = $disp to_show
                       pref = if to_show == 0 then "" else prefix
                       num_zeroes = ($w - length shown - length pref) `max` 0
                   in pref ++ replicate num_zeroes '0' ++ shown
                 |]
          err_letter = error "print_unsigned_int: Bad letter"

-- %e, E
print_exponent_double :: Char -> ConversionFunc
print_exponent_double e arg flags mw mp = res
    where preci = fromMaybe [| 6 |] mp
          width = fromMaybe [| 0 |] mw
          plus_sign = if Plus `elem` flags
                      then "+"
                      else if BlankPlus `elem` flags
                      then " "
                      else ""
          keep_dot = AlternateForm `elem` flags
          res = [| let to_show = (fromRational $ toRational $arg) :: Double
                       shown = showEFloat (Just $preci) (abs to_show) ""
                       sign = if to_show < 0 then "-" else plus_sign
                       fix_prec0 = if $preci == 0
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
                       num_zeroes = ($width - length fix_exp - length sign)
                              `max` 0
                   in sign ++ replicate num_zeroes '0' ++ fix_exp
                 |]

-- %f, F
print_fixed_double :: Char -> ConversionFunc
print_fixed_double f arg flags mw mp = res
    where preci = fromMaybe [| 6 |] mp
          width = fromMaybe [| 0 |] mw
          plus_sign = if Plus `elem` flags
                      then "+"
                      else if BlankPlus `elem` flags
                      then " "
                      else ""
          add_dot = AlternateForm `elem` flags
          fix_case | f == 'f'  = [| map toLower |]
                   | otherwise = [| map toUpper |]
          res = [| let to_show = (fromRational $ toRational $arg) :: Double
                       shown = showFFloat (Just $preci) (abs to_show) ""
                       shown' = if add_dot && $preci == 0 then shown ++ "."
                                                          else shown
                       sign = if to_show < 0 then "-" else plus_sign
                       num_zeroes = ($width - length shown' - length sign)
                              `max` 0
                   in sign ++ replicate num_zeroes '0' ++ $fix_case shown'
                 |]

-- %c, C
print_char :: ConversionFunc
print_char arg _ _ _ = [| [$arg] |]

-- %s, S
print_string :: ConversionFunc
print_string arg _ _ mp
    = case mp of
          Just preci -> [| if $preci < 0 then $arg else take $preci $arg |]
          Nothing -> arg

-- Corresponds to %H (Haskell extension)
show_arg :: ConversionFunc
show_arg arg flags mw mp = print_string [| show $arg |] flags mw mp

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
\end{code}

