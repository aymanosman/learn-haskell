.PHONY: binary.txt text.txt bench

all:
	stack ghc -- -O2 --make Bench.hs -rtsopts -o bench
	rm *.hi
	rm *.o

bench:
	@./bench binary-heap
	@echo
	@./bench pqueue
	@echo
	@./bench heap

judy:
	time ./bench judy +RTS -s

judy-direct:
	time ./bench judy-direct +RTS -s

heap:
	time ./bench heap +RTS -s

pqueue:
	time ./bench pqueue +RTS -s

binary.txt:
	time ./bench binary +RTS -s

text.txt:
	time ./bench text +RTS -s
