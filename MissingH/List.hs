{- arch-tag: List utilities main file
Copyright (C) 2004-2005 John Goerzen <jgoerzen@complete.org>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

{- |
   Module     : MissingH.List
   Copyright  : Copyright (C) 2004 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen, 
   Maintainer : jgoerzen@complete.org
   Stability  : provisional
   Portability: portable

This module provides various helpful utilities for dealing with lists.

Written by John Goerzen, jgoerzen\@complete.org
-}

module MissingH.List(-- * Tests
                     startswith, endswith, contains,
                     -- * Association List Utilities
                     {- | These functions are designed to augment the
                     association list functions in "Data.List" and
                     provide an interface similar to "Data.FiniteMap"
                     for association lists. -}
                     addToAL, delFromAL, flipAL, keysAL, valuesAL,
                     hasKeyAL,
                     -- ** Association List Conversions
                     strFromAL,
                     strToAL,
                     -- * Conversions
                     split, join, replace, genericJoin, takeWhileList,
                     dropWhileList, spanList, breakList,
                     -- * Advanced Conversions
                     WholeFunc(..), wholeMap, fixedWidth,
                     -- * Miscellaneous
                     countElem, elemRIndex, alwaysElemRIndex, seqList
                     -- -- * Sub-List Selection
                     -- sub,
                    ) where
import Data.List(intersperse, concat, isPrefixOf, isSuffixOf, elemIndices,
                elemIndex, elemIndices)
import IO
import System.IO.Unsafe

{- | Returns true if the given list starts with the specified elements;
false otherwise.  (This is an alias for "Data.List.isPrefixOf".)

Example:

> startswith "He" "Hello" -> True

-}

startswith :: Eq a => [a] -> [a] -> Bool
startswith = isPrefixOf

{- | Returns true if the given list ends with the specified elements;
false otherwise.  (This is an alias for "Data.List.isSuffixOf".)

Example:

> endswith "lo" "Hello" -> True

-}
endswith :: Eq a => [a] -> [a] -> Bool
endswith = isSuffixOf

{- | Similar to Data.List.takeWhile, takes elements while the func is true.
The function is given the remainder of the list to examine. -}
takeWhileList :: ([a] -> Bool) -> [a] -> [a]
takeWhileList _ [] = []
takeWhileList func list@(x:xs) =
    if func list 
       then x : takeWhileList func xs
       else []

{- | Similar to Data.List.dropWhile, drops elements while the func is true.
The function is given the remainder of the list to examine. -}
dropWhileList :: ([a] -> Bool) -> [a] -> [a]
dropWhileList _ [] = []
dropWhileList func list@(x:xs) =
    if func list
       then dropWhileList func xs
       else list

{- | Similar to Data.List.span, but performs the test on the entire remaining
list instead of just one element. 

@spanList p xs@ is the same as @(takeWhileList p xs, dropWhileList p xs)@ 
-}
spanList :: ([a] -> Bool) -> [a] -> ([a], [a])
spanList p xs = (takeWhileList p xs, dropWhileList p xs)

{- | Similar to Data.List.break, but performs the test on the entire remaining
list instead of just one element.
-}
breakList :: ([a] -> Bool) -> [a] -> ([a], [a])
breakList func = spanList (not . func)

{- | Given a delimiter and a list (or string), split into components.

Example:

> split "," "foo,bar,,baz," -> ["foo", "bar", "", "baz", ""]

> split "ba" ",foo,bar,,baz," -> [",foo,","r,,","z,"]
-}
split :: Eq a => [a] -> [a] -> [[a]]
split _ [] = []
split delim str =
    let (firstline, remainder) = breakList (startswith delim) str
        in 
        firstline : case remainder of
                                   [] -> []
                                   x -> if x == delim
                                        then [] : []
                                        else split delim 
                                                 (drop (length delim) x)


{- | Given a list and a replacement list, replaces each occurance of the search
list with the replacement list in the operation list.

Example:

>replace "," "." "127,0,0,1" -> "127.0.0.1"
-}

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace old new l = join new . split old $ l

{- | Given a delimiter and a list of items (or strings), join the items
by using the delimiter.

Example:

> join "|" ["foo", "bar", "baz"] -> "foo|bar|baz"
-}
join :: [a] -> [[a]] -> [a]
join delim l = concat (intersperse delim l)

{- | Like 'join', but works with a list of anything showable, converting
it to a String.

Examples:

> genericJoin ", " [1, 2, 3, 4] -> "1, 2, 3, 4"
> genericJoin "|" ["foo", "bar", "baz"] -> "\"foo\"|\"bar\"|\"baz\""

-}
genericJoin :: Show a => String -> [a] -> String
genericJoin delim l = join delim (map show l)

{- | Returns true if the given parameter is a sublist of the given list;
false otherwise.

Example:

> contains "Haskell" "I really like Haskell." -> True
> contains "Haskell" "OCaml is great." -> False
-}

contains :: Eq a => [a] -> [a] -> Bool
contains [] _ = True                    -- Sub is empty; matches anything
contains _ [] = False                   -- List is empty; matches nothing
contains sub searchlist =
    let testlist = take (length sub) searchlist
        in
        case sub == testlist of
                             True -> True
                             False -> contains sub (tail searchlist)

{- | Adds the specified (key, value) pair to the given list, removing any
existing pair with the same key already present. -}
addToAL :: Eq key => [(key, elt)] -> key -> elt -> [(key, elt)]
addToAL l key value = (key, value) : delFromAL l key

{- | Removes all (key, value) pairs from the given list where the key
matches the given one. -}
delFromAL :: Eq key => [(key, a)] -> key -> [(key, a)]
delFromAL l key = filter (\a -> (fst a) /= key) l

{- | Returns the keys that comprise the (key, value) pairs of the given AL.

Same as:

>map fst
-}
keysAL :: [(key, a)] -> [key]
keysAL = map fst

{- | Returns the values the comprise the (key, value) pairs of the given
AL.

Same as:

>map snd
-}
valuesAL :: [(a, value)] -> [value]
valuesAL = map snd

{- | Indicates whether or not the given key is in the AL. -}
hasKeyAL :: Eq a => a -> [(a, b)] -> Bool
hasKeyAL key list =
    elem key (keysAL list)

{- | Flips an association list.  Converts (key1, val), (key2, val) pairs
to (val, [key1, key2]). -}
flipAL :: (Eq key, Eq val) => [(key, val)] -> [(val, [key])]
flipAL oldl =
    let worker :: (Eq key, Eq val) => [(key, val)] -> [(val, [key])] -> [(val, [key])]
        worker [] accum = accum
        worker ((k, v):xs) accum =
            case lookup v accum of
                                Nothing -> worker xs ((v, [k]) : accum)
                                Just y -> worker xs (addToAL accum v (k:y))
        in
        worker oldl []

{- | Converts an association list to a string.  The string will have
one pair per line, with the key and value both represented as a Haskell string.

This function is designed to work with [(String, String)] association lists,
but may work with other types as well. -}

strFromAL :: (Show a, Show b) => [(a, b)] -> String
strFromAL inp =
    let worker (key, val) = show key ++ "," ++ show val
        in unlines . map worker $ inp

{- | The inverse of 'strFromAL', this function reads a string and outputs the
appropriate association list. 

Like 'strFromAL', this is designed to work with [(String, String)] association
lists but may also work with other objects with simple representations.
-}
strToAL :: (Read a, Read b) => String -> [(a, b)]
strToAL inp = 
    let worker line =
            case reads line of
               [(key, remainder)] -> case remainder of
                     ',':valstr -> (key, read valstr)
                     _ -> error "MissingH.List.strToAL: Parse error on value"
               _ -> error "MissingH.List.strToAL: Parse error on key"
        in map worker (lines inp)


{- FIXME TODO: sub -}

{- | Returns a count of the number of times the given element occured in the
given list. -}
countElem :: Eq a => a -> [a] -> Int
countElem i l = length (elemIndices i l)

{- | Returns the rightmost index of the given element in the
given list. -}
elemRIndex :: Eq a => a -> [a] -> Maybe Int
elemRIndex item l =
    case reverse $ elemIndices item l of
                                   [] -> Nothing
                                   (x:_) -> Just x
{- | Like elemRIndex, but returns -1 if there is nothing
found. -}
alwaysElemRIndex :: Eq a => a -> [a] -> Int
alwaysElemRIndex item list =
    case elemRIndex item list of
                              Nothing -> -1
                              Just x -> x

{- | Forces the evaluation of the entire list. -}
seqList :: [a] -> [a]
seqList [] = []
seqList (x:xs) = seq (seqList xs) (x:xs)

--------------------------------------------------
-- Advanced Conversions
--------------------------------------------------

{- | The type used for functions for 'wholeMap'.  See 'wholeMap' for details.
-}
newtype WholeFunc a b = WholeFunc ([a] -> (WholeFunc a b, [a], [b]))

{- | This is an enhanced version of the concatMap or map functions in 
Data.List.

Unlike those functions, this one:

 * Can consume a varying number of elements from the input list during
   each iteration

 * Can arbitrarily decide when to stop processing data

 * Can return a varying number of elements to insert into the output list

 * Can actually switch processing functions mid-stream

 * Is not even restricted to processing the input list intact

The function used by wholeMap, of type 'WholeFunc', is repeatedly called
with the input list.  The function returns three things: the function
to call for the next iteration (if any), what remains of the input list,
and the list of output elements generated during this iteration.  The return
value of 'wholeMap' is the concatenation of the output element lists from
all iterations.

Processing stops when the remaining input list is empty.  An example
of a 'WholeFunc' is 'fixedWidth'. -}
wholeMap :: WholeFunc a b -> [a] -> [b]
wholeMap _ [] = []              -- Empty input, empty output.
wholeMap (WholeFunc func) inplist =
    let (nextfunc, nextlist, output) = func inplist
        in
        output ++ wholeMap nextfunc nextlist

{- | A parser designed to process fixed-width input fields.  Use it with
'wholeMap'.

The Int list passed to this function is the list of the field widths desired
from the input.  The result is a list of those widths, if possible.  If any
of the input remains after processing this list, it is added on as the final
element in the result list.  If the input is less than the sum of the requested
widths, then the result list will be short the appropriate number of elements,
and its final element may be shorter than requested.

Examples:

>wholeMap (fixedWidth [1, 2, 3]) "1234567890"
> --> ["1","23","456","7890"]
>wholeMap (fixedWidth (repeat 2)) "123456789"
> --> ["12","34","56","78","9"]
>wholeMap (fixedWidth []) "123456789"
> --> ["123456789"]
>wholeMap (fixedWidth [5, 3, 6, 1]) "Hello, This is a test."
> --> ["Hello",", T","his is"," ","a test."]
-}
fixedWidth :: [Int] -> WholeFunc a [a]
fixedWidth len = 
    WholeFunc (fixedWidthFunc len)
    where -- Empty input: Empty output, stop
          fixedWidthFunc _ [] = ((fixedWidth []), [], [])
          -- Empty length: Stop here.
          fixedWidthFunc [] x = ((fixedWidth []), [], [x])
          -- Stuff to process: Do it.
          fixedWidthFunc (len:lenxs) input =
              (fixedWidth lenxs, next, [this])
              where (this, next) = splitAt len input
