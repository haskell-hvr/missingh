{- arch-tag: ConfigParser tests main file
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

module ConfigParser.Maintest(tests) where
import HUnit
import MissingH.ConfigParser
import MissingH.Either
import Testutil
import Control.Exception
import System.IO

nullfile = openFile "/dev/null" ReadWriteMode
testfile = "testsrc/ConfigParser/test.cfg"
p inp = forceEither $ readstring emptyCP inp
f msg inp exp conv = TestLabel msg $ TestCase $ assertEqual "" (Right exp) (conv (p inp))
f2 msg exp res = TestLabel msg $ TestCase $ assertEqual "" exp res
f3 msg inp exp conv = TestLabel msg $ TestCase $ assertEqual "" exp (conv (p inp))

test_basic =
        [
         f3 "empty doc, no sections" "" [] sections,
         f3 "one empty line" "\n" [] sections,
         f3 "comment line only" "#foo bar" [] sections,
         f3 "comment line with \\n" "#foo bar\n" [] sections,
         f3 "one empty sect" "[emptysect]" ["emptysect"] sections,
         f3 "one empty sect w comment" "#foo bar\n[emptysect]\n" ["emptysect"]
            sections,
         f3 "assure comments not processed"
            "# [nonexistant]\n[emptysect]\n" ["emptysect"] sections,
         f3 "1 empty s w/comments"
            "#fo\n[Cemptysect]\n#asdf boo\n  \n  # fnonexistantg"
            ["Cemptysect"] sections,
         f3 "1 empty s, comments, EOL"
            "[emptysect]\n# [nonexistant]\n" ["emptysect"] sections,
         TestLabel "1 sec w/option" $ TestCase $
           do let cp = p "[sect1]\nfoo: bar\n"
              ["sect1"] @=? sections cp
              (Right "bar") @=? get cp "sect1" "foo"
        , f "comments in option text"
            "[s1]\no1: v1#v2\n"
            "v1#v2" (\cp -> get cp "s1" "o1")
        , TestLabel "mult options" $ TestCase $
           do let cp = p "\n#foo\n[sect1]\n\n#iiii \no1: v1\no2:  v2\no3: v3"
              Right ["o1", "o2", "o3"] @=? options cp "sect1"
              ["sect1"] @=? sections cp
              Right "v2" @=? get cp "sect1" "o2"
        , TestLabel "sectionless option" $ TestCase $
           do let cp = p "v1: o1\n[sect1]\nv2: o2"
              Right "o1" @=? get cp "sect1" "v1"
              Right "o2" @=? get cp "sect1" "v2"
              Right "o1" @=? get cp "DEFAULT" "v1"
        , f3 "extensions to string" 
             "[sect1]\nfoo: bar\nbaz: l1\n l2\n   l3\n# c\nquux: asdf"
             "[sect1]\nbaz: l1\n    l2\n    l3\nfoo: bar\nquux: asdf\n\n"
             to_string
        ]


test_defaults = 
    let cp = p "def: ault\n[sect1]\nfoo: bar\nbaz: quuz\nint: 2\nfloat: 3\nbool: yes\n[sect4]\ndef: different" in
      [
       f2 "default item" (Right "ault") (get cp "sect1" "def")
      ,f2 "normal item" (Right "bar") (get cp "sect1" "foo")
      ,f2 "no option" (Left (NoOption "abc", "get")) (get cp "sect1" "abc")
      ,f2 "no section" (Left (NoSection "sect2", "get")) (get cp "sect2" "foo")
      ,f2 "default from bad sect" (Right "ault") (get cp "sect2" "def")
      ,f2 "overriding default" (Right "different") (get cp "sect4" "def")
      -- not in haskell: ,f2 "using default feature"
      -- default int
      -- default float
      -- default bool
      ]

test_nodefault =
    let cp = (p "def: ault\n[sect1]\nfoo: bar\nbaz: quuz\nint: 2\nfloat: 3\nbool: yes\n[sect4]\ndef: different"){usedefault = False} in
      [
       f2 "default item" (Left (NoOption "def", "get")) (get cp "sect1" "def")
      ,f2 "normal item" (Right "bar") (get cp "sect1" "foo")
      ,f2 "no option" (Left (NoOption "abc", "get")) (get cp "sect1" "abc")
      ,f2 "no section" (Left (NoSection "sect2", "get")) (get cp "sect2" "foo")
      ,f2 "default from bad sect" (Left (NoSection "sect2", "get")) (get cp "sect2" "def")
      ,f2 "overriding default" (Right "different") (get cp "sect4" "def")
      -- not in haskell: ,f2 "using default feature"
      -- default int
      -- default float
      -- default bool
      ]

test_ex_nomonad = 
    do 
       fh <- nullfile
       val <- readfile emptyCP testfile
       let cp = forceEither val
       hPutStr fh "Your setting is:"
       hPutStr fh $ forceEither $ get cp "file1" "location"

test_ex_errormonad = 
    [ 
      TestLabel "chaining1" $ TestCase $ 
      (Right ["opt1", "opt2"]) @=? 
       do let cp = emptyCP
          cp <- add_section cp "sect1"
          cp <- set cp "sect1" "opt1" "foo"
          cp <- set cp "sect1" "opt2" "bar"
          options cp "sect1"
     ,TestLabel "chaining2" $ TestCase $ 
      (Left (NoSection "sect2", "set")) @=? 
       do let cp = emptyCP
          cp <- add_section cp "sect1"
          cp <- set cp "sect1" "opt1" "foo"
          cp <- set cp "sect2" "opt2" "bar"
          options cp "sect1"
     ,TestLabel "chaining3" $ TestCase $ 
      ["opt1", "opt2"] @=? (
       forceEither $ do let cp = emptyCP
                        cp <- add_section cp "sect1"
                        cp <- set cp "sect1" "opt1" "foo"
                        cp <- set cp "sect1" "opt2" "bar"
                        options cp "sect1"
       )
    ]
     

tests = TestList [TestLabel "test_basic" (TestList test_basic),
                 TestLabel "test_defaults" (TestList test_defaults),
                 TestLabel "test_nodefault" (TestList test_nodefault),
                 TestLabel "test_ex_nomonad" (TestCase test_ex_nomonad),
                 TestLabel "test_ex_errormonad" (TestList test_ex_errormonad)]
