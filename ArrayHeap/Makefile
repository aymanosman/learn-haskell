all:
	ghc -O2 src/ArrayHeap.hs -rtsopts -o bin/foo \
		-prof -auto-all -caf-all -fforce-recomp

bench:
	bin/foo slow 1000000 +RTS -s -hd -p -i0.01
	hp2ps -e8in -c foo.hp
	mv foo.ps foo-slow.ps
	bin/foo fast 1000000 +RTS -s -hd -p -i0.001
	hp2ps -e8in -c foo.hp
	mv foo.ps foo-fast.ps
	rm foo.*
