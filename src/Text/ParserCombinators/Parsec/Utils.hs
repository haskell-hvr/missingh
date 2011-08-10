{- arch-tag: Parsec utilities
Copyright (c) 2004-2011 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file LICENSE
-}

{- |
   Module     : Text.ParserCombinators.Parsec.Utils
   Copyright  : Copyright (C) 2004-2011 John Goerzen
   License    : BSD3

   Maintainer : John Goerzen <jgoerzen@complete.org> 
   Stability  : provisional
   Portability: portable

Written by John Goerzen, jgoerzen\@complete.org

-}

module Text.ParserCombinators.Parsec.Utils(-- * Generalized Utilities
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
                             Just _ -> unexpected errormsg
        in
        try workerFunc

