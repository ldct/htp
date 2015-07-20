all:
	-mv build/*.o build/*.hi .
	ghc main.hs
	-mv *.o *.hi build

edit:
	vim -p *.hs
