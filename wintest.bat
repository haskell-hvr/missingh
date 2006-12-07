cd testsrc
ghc --make -package FilePath -package mtl -package HUnit -fallow-overlapping-instances -fallow-undecidable-instances -fglasgow-exts -cpp -o runtests.exe -i..\dist\build:..\src runtests.hs
cd ..
testsrc\runtests

