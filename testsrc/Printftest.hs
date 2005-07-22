{- arch-tag: Printf tests main file
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

module Printftest(tests) where
import Test.HUnit
import MissingH.Printf
import Data.FiniteMap

test_vsprintf = 
    map TestCase [
      "" @=? vsprintf ""
      ,"%" @=? vsprintf "%%"
      ,"asdf" @=? vsprintf "%s" "asdf"
      ,"foo: 5" @=? vsprintf "%s: %d" "foo" (5::Integer)
      ,"%foo%:% %-1%\n%" @=? vsprintf "%%%s%%:%% %%%d%%\n%%" "foo" (-1::Integer)
      ,"baz: 3.140000" @=? vsprintf "%s: %f" "baz" (3.14::Double)
      ,"quux: 3.140000e+02" @=? vsprintf "%s: %e" "quux" (314::Double)
      ,"fe" @=? vsprintf "%x" (254::Integer)
      ,"FE" @=? vsprintf "%X" (254::Integer)
      ,"10" @=? vsprintf "%o" (8::Integer)
      ,"Hello" @=? vsprintf "Hello"
      ,"Hello, John\n" @=? vsprintf "Hello, %s\n" "John"
      ,"John, your age is 10\n" @=? vsprintf "%s, your age is %d\n" "John" (10::Integer)
      ,"Hello" @=? sprintf "Hello" []
      ,"Hello, John\n" @=? sprintf "Hello, %s\n" [v "John"]
      ,"John, your age is 10\n" @=? sprintf "%s, your age is %d\n" [v "John",
                                                                 v (10::Integer)]
                           ]

test_al_fm =
    let testal = [("foo", v (1::Int)),
                  ("bar", v "asdf"),
                  ("baz", v (3.14::Double))]
        testfm = listToFM testal
        f exp inp = TestList $ [ 
                                TestCase $ exp @=? sprintfAL inp testal,
                                TestCase $ exp @=? sprintfFM inp testfm]
        in [
            f "" ""
           ,f "%" "%%"
           ,f "asdf" "%(bar)s"
           ,f "001" "%(foo)03d"
           ,f "asdf " "%(bar)-5s"
           ,f "3.140" "%(baz).3f"
           ,f "%asdf%" "%%%(bar)s%%"
           ,f "Str: asdf % Int: 1" "Str: %(bar)s %% Int: %(foo)d"
           ]

test_vsprintf_generics =
    map TestCase [
      "foo: 5" @=? vsprintf "%s: %d" "foo" (5::Int)
     ,"%foo%:% %-1%\n%" @=? vsprintf "%%%s%%:%% %%%d%%\n%%" "foo" (-1::Integer)
     ,"baz: 3.140000" @=? vsprintf "%s: %f" "baz" (3.14::Rational)
     ,"quux: 3.140000e+02" @=? vsprintf "%s: %e" "quux" (314::Double)
     ,"fe" @=? vsprintf "%x" (254::Int)
     ,"FE" @=? vsprintf "%X" (254::Int)
     ,"10" @=? vsprintf "%o" (8::Int)
     ,"10 3.140" @=? sprintf "%d %.3f" [v (10::Int), v (3.14::Float)]
                 ]

test_vsprintf_strings =
    map TestCase [
      ".     ." @=? vsprintf ".%5s." ""
     ,"     " @=? vsprintf "%5s" ""
     ,"     " @=? vsprintf "%-5s" ""
     ,"    x" @=? vsprintf "%5s" "x"
     ,"x    " @=? vsprintf "%-5s" "x"
     ,"abcde" @=? vsprintf "%.5s" "abcde"
     ,"abcde" @=? vsprintf "%.5s" "abcdef"
     ,"abcde" @=? vsprintf "%.5s" "abcdefghij"
     ,"abcde" @=? vsprintf "%5.5s" "abcdefg"
     ," abcde" @=? vsprintf "%6.5s" "abcdefg"
     ,"abcde " @=? vsprintf "%-6.5s" "abcdefg"
                 ]
    
  -- TODO: test numeric types  
    
tests = TestList [TestLabel "vsprintf" (TestList test_vsprintf),
                  TestLabel "vsprintf generics" (TestList test_vsprintf_generics),
                  TestLabel "vsprintf strings" (TestList test_vsprintf_strings),
                  TestLabel "vsprintf AL&FM" (TestList test_al_fm)
                 ]
