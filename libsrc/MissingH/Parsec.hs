{- arch-tag: Parsec utilities
Copyright (C) 2004 John Goerzen <jgoerzen@complete.org>

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
   Module     : MissingH.Parsec
   Copyright  : Copyright (C) 2004 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen, 
   Maintainer : jgoerzen@complete.org
   Stability  : provisional
   Portability: portable

Written by John Goerzen, jgoerzen\@complete.org

-}

module MissingH.Parsec(-- * Generalized Utilities
                       -- | These functions are generalized versions of
                       -- ones you might see in the Char parser.
                       GeneralizedToken, GeneralizedTokenParser,
                       togtok, tokeng, satisfyg, oneOfg, noneOfg,
                       specificg, allg,
                       -- * Other Utilities
                       notMatching
                      ) where

import Text.ParserCombinators.Parsec

type GeneralizedToken a = (SourcePos, a)
type GeneralizedTokenParser a st b = GenParser (GeneralizedToken a) st b

{- | Generate (return) a 'GeneralizedToken'. -}
togtok :: a -> GenParser b st (GeneralizedToken a)
togtok tok = do
              x <- getPosition
              return (x, tok)

{- | Retrieve the next token from a 'GeneralizedToken' stream.
   The given function should return the value to use, or Nothing
   to cause an error. -}
tokeng :: (Show a) => (a -> Maybe b) -> GeneralizedTokenParser a st b
tokeng test =
    token (show . snd) (fst) (test . snd)

{- | A shortcut to 'tokeng'; the test here is just a function that returns
a Bool.  If the result is true; return that value -- otherwise, an error.
-}
satisfyg :: (Show a) => (a -> Bool) -> GeneralizedTokenParser a st a
satisfyg test = tokeng (\t -> if test t then Just t else Nothing)

{- | Matches one item in a list and returns it. -}
oneOfg :: (Eq a, Show a) => [a] -> GeneralizedTokenParser a st a
oneOfg i = satisfyg (\x -> elem x i)

{- | Matches all items and returns them -}
allg :: (Show a) => GeneralizedTokenParser a st [a]
allg = many $ satisfyg (\_ -> True)

{- | Matches one item not in a list and returns it. -}
noneOfg :: (Eq a, Show a) => [a] -> GeneralizedTokenParser a st a
noneOfg l = satisfyg (\x -> not (elem x l))

{- | Matches one specific token and returns it. -}
specificg :: (Eq a, Show a) => a -> GeneralizedTokenParser a st a
specificg i = satisfyg (== i) <?> show i

{- Matches a list of tokens and returns it. -}
{-
listg :: (Eq a, Show a) => [GeneralizedToken a] -> GeneralizedTokenParser a st [GeneralizedToken a]
listg l = tokens (show . map fst) nextpos l
          where
          tokpos = fst
          nextpos
          nextposs _ _ (tok:toks) = tokpos tok
          nextposs _ tok [] = tokpos tok
          nextpos pos x = nextposs pos [x]
-}

{- | Running @notMatching p msg@ will try to apply parser p.
If it fails, returns ().  If it succeds, cause a failure and raise
the given error message.  It will not consume input in either case. -}
notMatching :: GenParser a b c -> String -> GenParser a b ()
notMatching p errormsg = 
    let maybeRead = try (do 
                         x <- p
                         return (Just x)
                        )
                    <|> return Nothing
        workerFunc =  do
                      x <- maybeRead
                      case x of
                             Nothing -> return ()
                             Just x -> unexpected errormsg
        in
        try workerFunc

