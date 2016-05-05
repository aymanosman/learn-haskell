Find largest element in collection of 1 million (i think)
499998508
binary-heap: 249.4 ms

499998508
pqueue: 232.5 ms

499998508
heap: 4.947 s


Cookie challenge on a collection of 100, 1000, and 10_000, respectively.
Showing VERY bad complexity...
30
binary-heap: 189.0 μs

286
binary-heap: 9.884 ms

3000
binary-heap: 3.069 s

And for 100_000, eek
30182
binary-heap: 714.1 s
 424,199,384,320 bytes allocated in the heap
 592,120,400,184 bytes copied during GC
      12,408,856 bytes maximum residency (40314 sample(s))
       4,316,840 bytes maximum slop
              37 MB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0     715103 colls,     0 par   306.836s  308.826s     0.0004s    0.0036s
  Gen  1     40314 colls,     0 par   280.740s  281.094s     0.0070s    0.0144s

  INIT    time    0.000s  (  0.000s elapsed)
  MUT     time  124.460s  (124.238s elapsed)
  GC      time  587.576s  (589.920s elapsed)
  EXIT    time    0.004s  (  0.002s elapsed)
  Total   time  712.040s  (714.160s elapsed)

  %GC     time      82.5%  (82.6% elapsed)

  Alloc rate    3,408,319,012 bytes per MUT second

  Productivity  17.5% of total user, 17.4% of total elapsed

# Some empirical orders of growth tests
With 100
30
binary-heap: 139.5 μs

With 1000
286
binary-heap: 10.65 ms

With 10,000
3000
binary-heap: 2.111 s

