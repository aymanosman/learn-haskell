build:
	ghc -O2 Foo.hs -o foo -rtsopts
	rm *.hi
	rm *.o

bench:
	@echo With 1000
	@./foo 1000
	@echo =============
	@echo With 100000
	@./foo 100000
	@echo =============
	@echo With 1000000
	@./foo 1000000
