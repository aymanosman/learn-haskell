.PHONY: binary.txt text.txt bench

all: build-bench2

# build-bench:
# 	stack ghc -- -O2 --make Bench.hs -rtsopts -o bench
# 	rm *.hi
# 	rm *.o

build-bench2:
	ghc -O2 src/Bench2.hs -rtsopts -o bin/bench2 -isrc
	# rm *.hi
	# rm *.o

build-bench3:
	stack ghc -- -O2 --make Bench3.hs -rtsopts -o bench3
	rm *.hi
	rm *.o

bench2:
	@echo With text100.txt
	@bin/bench2 binary-heap priv/text100.txt
	@echo
	@echo With text1_000.txt
	@bin/bench2 binary-heap priv/text1_000.txt
	@echo
	@echo With text10_000.txt
	@bin/bench2 binary-heap priv/text10_000.txt

heap:
	time ./bench heap +RTS -s

clean:
	rm *.hi
	rm *.o

core:
	ghc -O2 -ddump-simpl -dsuppress-idinfo -dsuppress-coercions\
		-dsuppress-type-applications -dsuppress-uniques -dsuppress-module-prefixes\
		Bench2.hs

sub2:
	ghc -O2 src/Sub2.hs -rtsopts -o bin/sub2 -isrc -prof -fprof-auto -caf-all

bench-sub2:
	bin/sub2 priv/text100.txt +RTS -s
	bin/sub2 priv/text1_000.txt +RTS -s
	bin/sub2 priv/text10_000.txt +RTS -s
	bin/sub2 priv/text100_000.txt +RTS -s

