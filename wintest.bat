cd testsrc
ghc --make -package mtl -package HUnit -fallow-overlapping-instances -fallow-undecidable-instances -fglasgow-exts -cpp -o runtests.exe -i..\dist\build:.. runtests.hs
cd ..

