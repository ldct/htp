all:
	-mv build/*.o build/*.hi .
	ghc main.hs -Wall
	-mv *.o *.hi build

edit:
	vim -p *.hs

clean:
	-rm build/*
