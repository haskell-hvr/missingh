{-# LANGUAGE CPP #-}

module TestUtils (mapassertEqual, assertRaises, errorCallMsg) where

import           Control.Exception (ErrorCall (..), Exception,
                                    Handler (Handler), SomeException, catches)
import           Test.HUnit        (Assertion, Test (TestCase), assertEqual,
                                    assertFailure)

mapassertEqual :: (Show b, Eq b) => String -> (a -> b) -> [(a, b)] -> [Test]
mapassertEqual label f xs
  = [ TestCase $ assertEqual label result (f inp) | (inp,result) <- xs ]


assertRaises :: (Exception e, Show e) => (e -> Bool) -> IO a -> Assertion
assertRaises check act = do
    res <- go `catches` [ Handler check', Handler anyEx ]
    res
  where
    go = act >> return (assertFailure "action completed without exception")

    check' ex
      | check ex  = return (return ())
      | otherwise = return (assertFailure ("got exception of expected type *but* wrong value: " ++ show ex))

    anyEx :: SomeException -> IO (Assertion)
    anyEx ex = return (assertFailure ("got unexpected exception type: " ++ show ex))


errorCallMsg :: ErrorCall -> String
errorCallMsg (ErrorCall msg)               = msg
#if MIN_VERSION_base(4,9,0)
errorCallMsg (ErrorCallWithLocation msg _) = msg
#endif
