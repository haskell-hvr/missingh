>-- #hide
> module Data.Hash.MD5.Zord64_HARD (Zord64) where

> import Data.Word
> import Data.Bits

> data Zord64 = W64 {lo,hi::Word32} deriving (Eq, Ord, Bounded)

> w64ToInteger W64{lo=lo,hi=hi} = toInteger lo + 0x100000000 * toInteger hi
> integerToW64 x = case x `quotRem` 0x100000000 of
>                  (h,l) -> W64{lo=fromInteger l, hi=fromInteger h}

> instance Show Zord64

> instance Read Zord64

> instance Num Zord64 where
>  W64{lo=lo_a,hi=hi_a} + W64{lo=lo_b,hi=hi_b} = W64{lo=lo', hi=hi'}
>   where lo' = lo_a + lo_b
>         hi' = hi_a + hi_b + if lo' < lo_a then 1 else 0
>  W64{lo=lo_a,hi=hi_a} - W64{lo=lo_b,hi=hi_b} = W64{lo=lo', hi=hi'}
>   where lo' = lo_a - lo_b
>         hi' = hi_a - hi_b + if lo' > lo_a then 1 else 0
>  fromInteger = integerToW64

> instance Bits Zord64 where
>  W64{lo=lo_a,hi=hi_a} .&. W64{lo=lo_b,hi=hi_b} = W64{lo=lo', hi=hi'}
>   where lo' = lo_a .&. lo_b
>         hi' = hi_a .&. hi_b
>  W64{lo=lo_a,hi=hi_a} .|. W64{lo=lo_b,hi=hi_b} = W64{lo=lo', hi=hi'}
>   where lo' = lo_a .|. lo_b
>         hi' = hi_a .|. hi_b
>  shift w 0 = w
>  shift W64{lo=lo,hi=hi} x
>   | x > 63 = W64{lo=0,hi=0}
>   | x > 31 = W64{lo = 0, hi = shift lo (x-32)}
>   | x > 0 = W64{lo = shift lo x, hi = shift hi x .|. shift lo (x-32)}
>   | x < -63 = W64{lo=0,hi=0}
>   | x < -31 = W64{lo = shift hi (x+32), hi = 0}
>   | x < 0 = W64{lo = shift lo x .|. shift hi (x+32), hi = shift hi x}
>  complement W64{lo=lo,hi=hi} = W64{lo=complement lo,hi=complement hi}

> instance Integral Zord64 where
>  toInteger = w64ToInteger

> instance Real Zord64
> instance Enum Zord64

