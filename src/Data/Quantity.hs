{-
Copyright (c) 2006-2011 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file LICENSE
-}

{- |
   Module     : Data.Quantity
   Copyright  : Copyright (C) 2006-2011 John Goerzen
   License    : BSD3

   Maintainer : John Goerzen <jgoerzen@complete.org> 
   Stability  : provisional
   Portability: portable

Tools for rendering sizes

Written by John Goerzen, jgoerzen\@complete.org -}

module Data.Quantity (
                          renderNum,
                          renderNums,
                          parseNum,
                          parseNumInt,
                          quantifyNum,
                          quantifyNums,
                          SizeOpts(..),
                          binaryOpts,
                          siOpts
                     )

where
import Data.List
import Text.Printf
import Data.Char

{- | The options for 'quantifyNum' and 'renderNum' -}
data SizeOpts = SizeOpts { base :: Int, -- ^ The base from which calculations are made
                           powerIncr :: Int, -- ^ The increment to the power for each new suffix
                           firstPower :: Int, -- ^ The first power for which suffixes are given
                           suffixes :: String -- ^ The suffixes themselves
                         }
                           
{- | Predefined definitions for byte measurement in groups of 1024, from 0 to
2**80 -}
binaryOpts :: SizeOpts
binaryOpts = SizeOpts {base = 2,
                       firstPower = 0,
                       suffixes = " KMGTPEZY",
                       powerIncr = 10}

{- | Predefined definitions for SI measurement, from 10**-24 to 10**24. -}
siOpts :: SizeOpts
siOpts = SizeOpts {base = 10,
                   firstPower = -24,
                   suffixes = "yzafpnum kMGTPEZY",
                   powerIncr = 3}

{- | Takes a number and returns a new (quantity, suffix) combination.
The space character is used as the suffix for items around 0. -}
quantifyNum :: (Ord a, Real a, Floating b, Ord b) => SizeOpts -> a -> (b, Char)
quantifyNum opts n = (\(x, s) -> (head x, s)) $ quantifyNums opts [n]

{- | Like 'quantifyNum', but takes a list of numbers.  The first number in
the list will be evaluated for the suffix.  The same suffix and scale will
be used for the remaining items in the list.  Please see 'renderNums' for
an example of how this works.

It is invalid to use this function on an empty list. -}
quantifyNums :: (Ord a, Real a, Floating b, Ord b) => SizeOpts -> [a] -> ([b], Char)
quantifyNums _ [] = error "Attempt to use quantifyNums on an empty list"
quantifyNums opts (headnum:xs) =
    (map (\n -> procnum n) (headnum:xs), suffix)
    where number = case fromRational . toRational $ headnum of
                     0 -> 1
                     x -> x
          incrList = map idx2pwr [0..length (suffixes opts) - 1]
          incrIdxList = zip incrList [0..]
          idx2pwr i = i * powerIncr opts + firstPower opts
          finderfunc (x, _) = (fromIntegral $ base opts) ** (fromIntegral x) 
                              <= (abs number)
          -- Find the largest item that does not exceed the number given.
          -- If the number is larger than the larger item in the list,
          -- that's fine; we'll just write it in terms of what we have.

          (usedexp, expidx) =
              case find finderfunc (reverse incrIdxList) of
                  Just x -> x
                  Nothing -> head incrIdxList -- If not found, it's smaller than the first
          suffix = (suffixes opts !! (fromIntegral expidx))
          procnum n = (fromRational . toRational $ n) /
                      ((fromIntegral (base opts) ** (fromIntegral usedexp)))
          --(posres, possuf) = quantifyNum opts (headnum * (-1))

{- | Render a number into a string, based on the given quantities.  This is
useful for displaying quantities in terms of bytes or in SI units.  Give this
function the 'SizeOpts' for the desired output, and a precision (number of
digits to the right of the decimal point), and you get a string output.

Here are some examples:

> Data.Quantity> renderNum binaryOpts 0 1048576
> "1M"
> Data.Quantity> renderNum binaryOpts 2 10485760
> "10.00M"
> Data.Quantity> renderNum binaryOpts 3 1048576
> "1.000M"
> Data.Quantity> renderNum binaryOpts 3 1500000
> "1.431M"
> Data.Quantity> renderNum binaryOpts 2 (1500 ** 3)
> "3.14G"

> Data.Quantity> renderNum siOpts 2 1024
> "1.02k"
> Data.Quantity> renderNum siOpts 2 1048576
> "1.05M"
> Data.Quantity> renderNum siOpts 2 0.001
> "1.00m"
> Data.Quantity> renderNum siOpts 2 0.0001
> "100.00u"

If you want more control over the output, see 'quantifyNum'. -}
renderNum :: (Ord a, Real a) => 
             SizeOpts
          -> Int                -- ^ Precision of the result
          -> a                  -- ^ The number to examine
          -> String
renderNum opts prec number =
    (printf ("%." ++ show prec ++ "g") num) ++ [suffix]
    where (num, suffix) = (quantifyNum opts number)::(Double, Char)

{- | Like 'renderNum', but operates on a list of numbers.  The first number
in the list will be evaluated for the suffix.  The same suffix and scale will
be used for the remaining items in the list.  See 'renderNum' for more
examples.

Also, unlike 'renderNum', the %f instead of %g printf format is used so that
\"scientific\" notation is avoided in the output.

Examples:

> *Data.Quantity> renderNums binaryOpts 3 [1500000, 10240, 104857600]
> ["1.431M","0.010M","100.000M"]
> *Data.Quantity> renderNums binaryOpts 3 [1500, 10240, 104857600]
> ["1.465K","10.000K","102400.000K"]

-}
renderNums :: (Ord a, Real a) =>
              SizeOpts
           -> Int               -- ^ Prevision of the result
           -> [a]               -- ^ The numbers to examine
           -> [String]          -- ^ Result
renderNums opts prec numbers =
    map printit convnums
    where printit num =
              (printf ("%." ++ show prec ++ "f") num) ++ [suffix]
          (convnums, suffix) = 
              (quantifyNums opts numbers)::([Double], Char)

{- | Parses a String, possibly generated by 'renderNum'.  Parses the suffix
and applies it to the number, which is read via the Read class.

Returns Left "error message" on error, or Right number on successful parse.

If you want an Integral result, the convenience function 'parseNumInt' is for
you.
-}
parseNum :: (Read a, Fractional a) => 
            SizeOpts            -- ^ Information on how to parse this data
         -> Bool                -- ^ Whether to perform a case-insensitive match
         -> String              -- ^ The string to parse
         -> Either String a
parseNum opts insensitive inp =
    case reads inp of
      [] -> Left "Couldn't parse numeric component of input"
      [(num, "")] -> Right num  -- No suffix; pass number unhindered
      [(num, [suffix])] ->
          case lookup (caseTransformer suffix) suffixMap of
            Nothing -> Left $ "Unrecognized suffix " ++ show suffix
            Just power -> Right $ num * multiplier power
      [(_, suffix)] -> Left $ "Multi-character suffix " ++ show suffix
      _ -> Left "Multiple parses for input"
    where suffixMap = zip (map caseTransformer . suffixes $ opts) 
                          (iterate (+ (powerIncr opts)) (firstPower opts))
          caseTransformer x
              | insensitive = toLower x
              | otherwise = x
          multiplier :: (Read a, Fractional a) => Int -> a
          multiplier power =
              fromRational . toRational $ 
                           fromIntegral (base opts) ** fromIntegral power
{- | Parse a number as with 'parseNum', but return the result as
an 'Integral'.  Any type such as Integer, Int, etc. can be used for the
result type.

This function simply calls 'round' on the result of 'parseNum'.  A
'Double' is used internally for the parsing of the numeric component.

By using this function, a user can still say something like 1.5M and get an
integral result. -}
parseNumInt :: (Read a, Integral a) => 
               SizeOpts         -- ^ Information on how to parse this data
            -> Bool             -- ^ Whether to perform a case-insensitive match
            -> String           -- ^ The string to parse
            -> Either String a
parseNumInt opts insensitive inp =
    case (parseNum opts insensitive inp)::Either String Double of
      Left x -> Left x
      Right n -> Right (round n)