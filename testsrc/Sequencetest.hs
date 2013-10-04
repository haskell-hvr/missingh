module Sequencetest(tests) where
import Data.Sequence (Seq)
import qualified Data.Sequence.Utils as S
import qualified Data.Sequence as S
import Test.HUnit
import Test.HUnit.Tools

prop_takeR n l =
  (S.reverse . S.take n . S.reverse $ s) == (S.takeR n s) where
    s = S.fromList l :: Seq Bool

prop_dropR n l =
  (S.reverse . S.drop n . S.reverse $ s) == (S.dropR n s) where
    s = S.fromList l :: Seq Bool

tests = TestList [qctest "takeR" prop_takeR,
                  qctest "dropR" prop_dropR]
