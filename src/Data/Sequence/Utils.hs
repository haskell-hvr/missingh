module Data.Sequence.Utils (takeR, dropR) where

import Data.Sequence (Seq)
import qualified Data.Sequence as S

{- | /O(log(min(i,n-1)))/. The last @i@ elements of a sequence.
     If @i@ is negative, @'takeR' i s@ yields the empty sequence.
     If the sequence contains fewer than @i@ elements, the whole
     sequence is returned.

     @'takeR' i@ is equivalent to @'reverse' . ('take' i) . 'reverse'@. -}
takeR     :: Int -> Seq a -> Seq a
takeR i s =  S.drop (subtract i $ S.length s) s

{- | /O(log(min(i,n-1)))/. Elements of a sequence before the last @i@.
     If @i@ is negative, @'dropR' i s@ yields the whole sequence.
     If the sequence contains fewer than @i@ elements, the empty
     sequence is returned.

     @'dropR' i@ is equivalent to @'reverse' . ('drop' i) . 'reverse'@. -}
dropR     :: Int -> Seq a -> Seq a
dropR i s = S.take (subtract i $ S.length s) s
