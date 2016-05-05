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

build-heapsort:
	ghc -O2 HeapSort.hs -o heapsort -rtsopts
	rm *.hi
	rm *.o

bench-heapsort:
	@echo With 1000
	@./heapsort all 1000
	@echo =============
	@echo With 10,000
	@./heapsort all 10000
	@echo =============
	@echo With 100,000
	@./heapsort all 100000

# bench-heapsort:
# 	@echo With 1000
# 	@./heapsort list 1000
# 	@./heapsort array 1000
# 	@./heapsort array2 1000
# 	@echo =============
# 	@echo With 10,000
# 	@./heapsort list 10000
# 	@./heapsort array 10000
# 	@./heapsort array2 10000
# 	@echo =============
# 	@echo With 100,000
# 	@./heapsort list 100000
# 	@./heapsort array 100000
# 	@./heapsort array2 100000

