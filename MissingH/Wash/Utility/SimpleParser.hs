-- © 2002 Peter Thiemann
module MissingH.Wash.Utility.SimpleParser where

import Char

-- very simple parser combinators: Parsec is too sophisticated!
newtype Parser a b = Parser (a -> [(b, a)])
unParser (Parser g) = g
instance Monad (Parser a) where
  return x = Parser (\ w -> [(x, w)])
  m >>=  f = let g = unParser m in
	     Parser (\ w -> [ (y, w'') | (x, w') <- g w, (y, w'') <- unParser (f x) w'])
  fail str = Parser (\ w -> [])

satisfy p = Parser (\ w -> [(x, w') | x:w' <- [w], p x])

print = satisfy isPrint
alphaNum = satisfy isAlphaNum
alpha = satisfy isAlpha
ascii = satisfy isAscii
digit = satisfy isDigit
char c = satisfy (==c)
string s = foldr (\ x p -> do { c <- char x; cs <- p; return (c:cs); }) (return "") s
oneOf cs = satisfy (`elem` cs)
noneOf cs = satisfy (not . (`elem` cs))

eof = Parser (\ w -> if null w then [((),[])] else [])
try parser = parser
p1 <|> p2 = let g1 = unParser p1
		g2 = unParser p2
	    in  Parser (\w -> g1 w ++ g2 w)

option :: x -> Parser a x -> Parser a x
option x parser = parser <|> return x

many1 p = 
  do x <- p
     xs <- many p
     return (x : xs)

many p = 
  option [] (many1 p)

manyn n p =
  if n <= 0 
  then return []
  else do x <- p
	  xs <- manyn (n-1) p
	  return (x : xs)


parseFromString :: Parser String x -> String -> Maybe x
parseFromString parser str =
  let g = unParser (parser >>= (\x -> eof >> return x)) in
  case g str of
    (x, ""): _ -> Just x
    _ -> Nothing

parserToRead :: Parser String x -> ReadS x
parserToRead parser = unParser parser
