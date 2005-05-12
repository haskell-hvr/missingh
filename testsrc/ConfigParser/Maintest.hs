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
import Test.HUnit
import MissingH.ConfigParser
import MissingH.Either
import Testutil
import Control.Exception
import System.IO

nullfile = openFile "/dev/null" ReadWriteMode
testfile = "testsrc/ConfigParser/test.cfg"
p inp = forceEither $ readstring emptyCP inp
f msg inp exp conv = TestLabel msg $ TestCase $ assertEqual "" (Right exp) (conv (p inp))

-- f2s, f2b are useful for matching Left return values
f2s :: String -> Either CPError String -> Either CPError String -> Test
f2s = f2
f2b :: String -> Either CPError Bool -> Either CPError Bool -> Test
f2b = f2

f2 msg exp res = TestLabel msg $ TestCase $ assertEqual "" exp res
f3 msg inp exp conv = TestLabel msg $ TestCase $ assertEqual "" exp (conv (p inp))

instance Show ConfigParser where
    show x = show (content x)

instance Eq ConfigParser where
    x == y = (content x) == (content y)

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
      ,f2s "no option" (Left (NoOption "abc", "get")) (get cp "sect1" "abc")
      ,f2s "no section" (Left (NoSection "sect2", "get")) (get cp "sect2" "foo")
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
       f2s "default item" (Left (NoOption "def", "get")) (get cp "sect1" "def")
      ,f2 "normal item" (Right "bar") (get cp "sect1" "foo")
      ,f2s "no option" (Left (NoOption "abc", "get")) (get cp "sect1" "abc")
      ,f2s "no section" (Left (NoSection "sect2", "get")) (get cp "sect2" "foo")
      ,f2s "default from bad sect" (Left (NoSection "sect2", "get")) (get cp "sect2" "def")
      ,f2 "overriding default" (Right "different") (get cp "sect4" "def")
      -- not in haskell: ,f2 "using default feature"
      -- default int
      -- default float
      -- default bool
      ]

test_instances = 
    let cp = p "[x]\na: true\nb: 1\nbad: never"
	in [f2 "bool 1st" (Right True) (get cp "x" "a"),
	    f2 "bool 1nd" (Right True) (get cp "x" "b"),
            f2b "bad bool" (Left (ParseError "couldn't parse bool never from x/bad", "getbool")) (get cp "x" "bad"),
            f2 "number" (Right (1::Int)) (get cp "x" "b")
	   ]


test_remove = 
    let cp = forceEither $ readstring emptyCP "def:ault\n[sect1]\ns1o1: v1\ns1o2:v2\n[sect2]\ns2o1: v1\ns2o2: v2\n[sect3]"
        in [
            f2 "setup" ["sect1", "sect2", "sect3"] (sections cp)
           ,f2 "remove 1st s" (Right ["sect2", "sect3"])
               (do x <- remove_section cp "sect1"
                   return $ sections x
                )
           ,f2 "remove 2nd s" (Right ["sect1", "sect3"])
               (do x <- remove_section cp "sect2"
                   return $ sections x
                )
           ,f2 "remove 3rd s" (Right ["sect1", "sect2"])
               (do x <- remove_section cp "sect3"
                   return $ sections x
                )
           ,f2 "error handling s" (Left (NoSection "sect4", "remove_section"))
                  (remove_section cp "sect4")
           ,f2 "remove an option" (Right (["sect1", "sect2", "sect3"], ["s1o2"]))
               (do x <- remove_option cp "sect1" "s1o1"
                   y <- options x "sect1"
                   return (sections x, y)
                )
           ,f2 "option err 1" (Left (NoSection "sect4", "remove_option"))
               (remove_option cp "sect4" "s4o1")
           ,f2 "option err 2" (Left (NoOption "s1o3", "remove_option"))
               (remove_option cp "sect1" "s1o3")
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

test_interp = 
    let interpdoc = "[DEFAULT]\narch = i386\n\n[builder]\n" ++
                    "filename = test_%(arch)s.c\n" ++
                    "dir = /usr/src/%(filename)s\n" ++
                    "percent = 5%%\n" ++
                    "bad = /usr/src/%(nonexistent)s\n" ++
                    "recursive = foo%(recursive)s\n" ++
                    "syn1 = foo%()s\n" ++
                    "syn2 = foo%(asdf)\n" ++
                    "syn3 = foo%s\n" ++
                    "syn4 = %\n"
        cp = (forceEither $ (readstring emptyCP interpdoc)){ accessfunc = interpolatingAccess 5}
        in
        [
         f2 "basic" (Right "i386") (get cp "DEFAULT" "arch")
        ,f2 "filename" (Right "test_i386.c") (get cp "builder" "filename")
        ,f2 "dir" (Right "/usr/src/test_i386.c") (get cp "builder" "dir")
        ,f2 "percents" (Right "5%") (get cp "builder" "percent")
        ,f2s "error" (Left (InterpolationError "unresolvable interpolation reference to \"nonexistent\"", "interpolatingAccess")) (get cp "builder" "bad")
        ,f2s "recursive" (Left (InterpolationError "maximum interpolation depth exceeded", "interpolatingAccess"))
                        (get cp "builder" "recursive")
        ,f2s "syn1" (Left (InterpolationError "\"builder/syn1\" (line 1, column 6):\nunexpected \")\"\nexpecting interpolation name","interpolatingAccess"))
                   (get cp "builder" "syn1")
        ,f2s "syn2" (Left (InterpolationError "\"builder/syn2\" (line 1, column 10):\nunexpected end of input\nexpecting \")s\"","interpolatingAccess"))
                   (get cp "builder" "syn2")
        ,f2s "syn3" (Left (InterpolationError "\"builder/syn3\" (line 1, column 4):\nunexpected \"s\"\nexpecting \"%(\"","interpolatingAccess"))
                   (get cp "builder" "syn3")
        ,f2s "syn4" (Left (InterpolationError "\"builder/syn4\" (line 1, column 1):\nunexpected end of input\nexpecting \"%(\"","interpolatingAccess"))
                   (get cp "builder" "syn4")
        ]


tests = TestList [TestLabel "test_basic" (TestList test_basic),
                 TestLabel "test_defaults" (TestList test_defaults),
                 TestLabel "test_nodefault" (TestList test_nodefault),
                 TestLabel "test_remove" (TestList test_remove),
                 TestLabel "test_ex_nomonad" (TestCase test_ex_nomonad),
                 TestLabel "test_ex_errormonad" (TestList test_ex_errormonad),
                 TestLabel "test_interp" (TestList test_interp),
                 TestLabel "test_instances" (TestList test_instances)]

