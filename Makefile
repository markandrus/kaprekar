all: kaprekar.hs
	ghc -O --make kaprekar.hs && time ./kaprekar

profile: kaprekar.hs
	ghc -O --make -rtsopts=all -prof -auto-all kaprekar.hs && time ./kaprekar +RTS -h

clean:
	rm kaprekar
	rm kaprekar.o
	rm kaprekar.hi
