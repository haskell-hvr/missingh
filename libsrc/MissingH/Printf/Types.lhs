arch-tag: Printf type declarations

\begin{code}
module Types where

import Language.Haskell.THSyntax

data Format = Literal String
            | Conversion ExpQ
            | CharCount

type ArgNum = Integer
type Arg = ExpQ
type Width = ExpQ
type Precision = ExpQ
data Flag = AlternateForm       -- "#"
          | ZeroPadded          -- "0"
          | LeftAdjust          -- "-"
          | BlankPlus           -- " "
          | Plus                -- "+"
          | Thousands           -- "'"
          | AlternativeDigits   -- "I" (ignored)
    deriving (Eq, Show)

xvar :: ArgNum -> String
xvar i = 'x':show i

yvar :: ArgNum -> String
yvar i = 'y':show i

nvar :: ArgNum -> String
nvar i = 'n':show i
\end{code}

