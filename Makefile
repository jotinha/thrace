all: 
	ghc -O2 -XCPP main.hs
	time ./main > temp.ppm
	mv temp.ppm test.ppm
