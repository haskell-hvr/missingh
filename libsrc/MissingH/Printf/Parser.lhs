arch-tag: Printf parser declarations

\begin{code}
module Parser (parse) where

import Language.Haskell.THSyntax
import Maybe (isJust, fromJust)
import Printer (get_conversion_func)
import Char (isDigit)
import List (nub, delete)
import Types

{-
xn where n is an integer refers to an argument to the function
y*, n* is reserved for %n
fw* is reserved for field width intermediates
Everything else can be used by the conversion functions
-}

-- parse takes a format string and returns a list of Formats with which
-- to build the output and the number of arguments to take
parse :: String -> ([Format], ArgNum)
parse = parse' 1 0

parse' :: ArgNum     -- The next argument number
       -> ArgNum     -- The maximum numbered argument number used so far
       -> String     -- The format string
       -> ([Format], -- The bits of output
           ArgNum)   -- The number of arguments to take
-- When we are at the end of the input there are no more bits of input
-- remaining. The number of arguments is the largest argument used.
parse' x_var max_x_var "" = ([], (x_var - 1) `max` max_x_var)
parse' x_var max_x_var xs
  = case parse_format x_var xs of
        (f, x_var', used, xs') ->
            case parse' x_var' (maximum (max_x_var:used)) xs' of
                (fs, final_max_x_var) -> (f:fs, final_max_x_var)

parse_format :: ArgNum     -- The next argument to use
             -> String     -- The format string
             -> (Format,   -- The Format we put together
                 ArgNum,   -- The new next argument to use
                 [ArgNum], -- The argument numbers we've used
                 String)   -- The remainder of the format string
parse_format x_var ('%':xs)
    = case conv_spec of
          'n' -> (CharCount, x_var, [], xs5)
          '%' -> (Literal "%", x_var, [], xs5)
          _   -> (Conversion converted', x_var0, used, xs5)
    where (arg, used0, x_var0, xs0) = get_arg x_var3 xs
          (flags, xs1) = get_flags xs0
          flags' = if isJust mprec then delete ZeroPadded flags else flags
          (mfw, used2, x_var2, xs2) = get_min_field_width x_var xs1
          (mprec, used3, x_var3, xs3) = get_precision x_var2 xs2
          (_length_mod, xs4) = get_length_modifier xs3
          (conv_spec, xs5) = get_conversion_specifier xs4
          conv_func = get_conversion_func conv_spec
          used = used0 ++ used2 ++ used3
          converted = conv_func arg flags' mfw mprec
          converted' = fix_width flags' mfw converted
parse_format x_var xs = case break ('%' ==) xs of
                            (ys, zs) -> (Literal ys, x_var, [], zs)

fix_width :: [Flag] -> Maybe Width -> ExpQ -> ExpQ
fix_width _ Nothing e = e
fix_width flags (Just w) e = letE [dec_e] exp_spaced
    where 
          dec_e = valD (varP "e") (normalB e) []
          exp_num_spaces = [| abs $w - length $e |]
          exp_num_spaces' = [| 0 `max` $exp_num_spaces |]
          exp_spaces = [| replicate $exp_num_spaces' ' ' |]
          exp_left_padded = [| $(varE "e") ++ $exp_spaces |]
          exp_right_padded = [| $exp_spaces ++ $(varE "e") |]
          exp_spaced = if LeftAdjust `elem` flags
                       then exp_left_padded
                       else [| if $w < 0 then $exp_left_padded
                                         else $exp_right_padded |]

get_flags :: String -> ([Flag], String)
get_flags s = (flags'', s')
    where (cs, s') = span (`elem` "#0- +'I") s
          unique_cs = nub cs
          flags = map (fromJust . (`lookup` flag_mapping)) unique_cs
          flags' = if LeftAdjust `elem` flags then filter (/= ZeroPadded) flags
                                              else flags
          flags'' = if Plus `elem` flags then filter (/= BlankPlus) flags
                                         else flags'
          flag_mapping = [('#', AlternateForm),
                          ('0', ZeroPadded),
                          ('-', LeftAdjust),
                          (' ', BlankPlus),
                          ('+', Plus),
                          ('\'', Thousands),
                          ('I', AlternativeDigits)]

get_min_field_width :: ArgNum
                    -> String
                    -> (Maybe Width, [ArgNum], ArgNum, String)
get_min_field_width x_var s
 = case get_num s of
       Just (n, s') -> (Just [| n |], [], x_var, s')
       Nothing -> case get_star_arg x_var s of
                      Just (a, used, x_var', s') -> (Just a, used, x_var', s')
                      Nothing -> (Nothing, [], x_var, s)

-- Need to check prec >= 0 at some point?

get_precision :: ArgNum
              -> String
              -> (Maybe Precision, [ArgNum], ArgNum, String)
get_precision x_var ('.':s)
 = case get_num s of
       Just (n, s') -> (Just [| n |], [], x_var, s')
       Nothing -> case get_star_arg x_var s of
                      Just (a, used, x_var', s') -> (Just a, used, x_var', s')
                      Nothing -> (Just [| 0 |], [], x_var, s)
get_precision x_var s = (Nothing, [], x_var, s)

get_star_arg :: ArgNum -> String -> Maybe (Arg, [ArgNum], ArgNum, String)
get_star_arg x_var ('*':s) = Just (get_arg x_var s)
get_star_arg _ _ = Nothing

get_arg :: ArgNum -> String -> (Arg, [ArgNum], ArgNum, String)
get_arg x_var s = case get_num s of
                      Just (i, '$':s') -> (varE (xvar i), [i], x_var, s')
                      _ -> (varE (xvar x_var), [], x_var + 1, s)

get_num :: String -> Maybe (Integer, String)
get_num s = case span isDigit s of
                ("", _) -> Nothing
                (xs, s') -> Just ((read xs), s')

get_length_modifier :: String -> (String, String)
get_length_modifier s
    | take 2 s `elem` ["hh", "ll"]                        = splitAt 2 s
    | take 1 s `elem` ["h", "l", "L", "q", "j", "z", "t"] = splitAt 1 s
    | otherwise                                           = ("", s)

get_conversion_specifier :: String -> (Char, String)
get_conversion_specifier (x:xs) = (x, xs) -- XXX errors
get_conversion_specifier "" = error "Printf: get_conversion_specifier \"\""
\end{code}

