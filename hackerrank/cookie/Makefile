all: build-bench2

build-bench2:
	ghc -O2 src/Bench2.hs -rtsopts -o bin/bench2 -isrc

build-bench3:
	stack ghc -- -O2 --make Bench3.hs -rtsopts -o bench3

bench2:
	@echo With text100.txt
	@bin/bench2 binary-heap priv/text100.txt
	@echo
	@echo With text1_000.txt
	@bin/bench2 binary-heap priv/text1_000.txt
	@echo
	@echo With text10_000.txt
	@bin/bench2 binary-heap priv/text10_000.txt

sub2:
	ghc -O2 src/Sub2.hs -rtsopts -o bin/sub2 -isrc -prof -fprof-auto -caf-all

bench-sub2:
	bin/sub2 priv/text100.txt +RTS -s
	bin/sub2 priv/text1_000.txt +RTS -s
	bin/sub2 priv/text10_000.txt +RTS -s

bench-sub2-short:
	time bin/sub2 priv/text100.txt
	@echo
	@echo
	time bin/sub2 priv/text1_000.txt
	@echo
	@echo
	time bin/sub2 priv/text10_000.txt
	@echo
	@echo
	time bin/sub2 priv/text100_000.txt

with-stack:
	stack ghc -- -O2 src/Sub1.hs -o foo -prof -auto-all -caf-all -fforce-recomp
