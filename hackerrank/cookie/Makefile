.PHONY: binary.txt text.txt bench

all: build-bench build-bench2

build-bench:
	stack ghc -- -O2 --make Bench.hs -rtsopts -o bench
	rm *.hi
	rm *.o

build-bench2:
	stack ghc -- -O2 --make Bench2.hs -rtsopts -o bench2
	rm *.hi
	rm *.o

bench:
	@./bench binary-heap
	@echo
	@./bench pqueue
	@echo
	@./bench heap

heap:
	time ./bench heap +RTS -s

pqueue:
	time ./bench pqueue +RTS -s

text.txt:
	time ./bench text +RTS -s
