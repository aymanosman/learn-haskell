all:
	stack ghc -- -O2 --make Bench.hs -rtsopts -o bench
	rm *.hi
	rm *.o

judy:
	time ./bench judy +RTS -s

judy-direct:
	time ./bench judy-direct +RTS -s

heap:
	time ./bench heap +RTS -s

pqueue:
	time ./bench pqueue +RTS -s

.PHONY: binary.txt text.txt

binary.txt:
	time ./bench binary +RTS -s

text.txt:
	time ./bench text +RTS -s
