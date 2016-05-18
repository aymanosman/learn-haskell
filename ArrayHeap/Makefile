all:
	ghc -O2 src/ArrayHeap.hs -rtsopts -o bin/foo \
		-prof -auto-all -caf-all -fforce-recomp

bench:
	bin/foo slow 1000000 +RTS -s -hd -p -i0.01
