all:
	stack ghc -- -O2 --make Bench.hs -rtsopts -o bench
	rm *.hi
	rm *.o

judy:
	time ./bench judy +RTS -s

heap:
	time ./bench heap +RTS -s

