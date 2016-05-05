./heapsort all 10
7291
list: 213.3 μs
121156
array: 31.09 μs
174775
array2: 275.4 μs

./heapsort all 100
85
list: 706.7 μs
7381
array: 257.6 μs
6343
array2: 5.388 ms

./heapsort all 1000
515
list: 756.9 μs
1168
array: 768.3 μs
72
array2: 20.22 ms

./heapsort all 10000
5
list: 4.774 ms
31
array: 6.887 ms
31
array2: 269.5 ms

./heapsort all 100000
26
list: 59.49 ms
4
array: 95.78 ms
4
array2: 3.526 s

# Second Test, factoring out random number generation

With 1000
844
list: 258.0 μs
844
array: 790.1 μs
844
array2: 57.90 ms

With 10,000
105
list: 2.871 ms
105
array: 5.375 ms
105
array2: 270.6 ms

With 100,000
6
list: 25.16 ms
6
array: 57.49 ms
6
array2: 3.527 s

