class Heap(object):
  def __init__(self):
      """TODO: to be defined1. """
      self.arr = [None]

  def __str__(self):
      return "Heap(%s)" % str(self.to_list())

  def insert(self, a):
      self.arr.append(a)
      self.heapify(len(self.arr) - 1)

  def to_list(self):
      return self.arr[1:]

  def swap(self, i, j):
      a = self.arr[i]
      self.arr[i] = self.arr[j]
      self.arr[j] = a

  def heapify(self, i):
      parent = i/2
      while parent != 0 and self.arr[i] < self.arr[parent]:
          self.swap(i, parent)
          i = parent
          parent = i/2
          print "parent(%s)" % parent


h = Heap()
h.insert(3)
h.insert(2)
h.insert(1)

print(h)
print 3/2
