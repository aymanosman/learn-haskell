.PHONY: binary.txt text.txt bench

all: build-bench2

# build-bench:
# 	stack ghc -- -O2 --make Bench.hs -rtsopts -o bench
# 	rm *.hi
# 	rm *.o

build-bench2:
	stack ghc -- -O2 --make Bench2.hs -rtsopts -o bench2
	rm *.hi
	rm *.o

bench:
	@./bench2 binary-heap text100.txt
	@echo
	@./bench2 binary-heap text1_000.txt
	@echo
	@./bench2 binary-heap text10_000.txt

heap:
	time ./bench heap +RTS -s

clean:
	rm *.hi
	rm *.o

core:
	ghc -O2 -ddump-simpl -dsuppress-idinfo -dsuppress-coercions\
		-dsuppress-type-applications -dsuppress-uniques -dsuppress-module-prefixes\
		Bench2.hs
