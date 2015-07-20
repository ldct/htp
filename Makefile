all:
	mv build/*.o .
	ghc main.hs
	mv *.o *.hi build
#	make clean

clean:
	rm *.o *.hi

edit:
	vim -p *.hs
