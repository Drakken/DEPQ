
DEPQ: double-ended priority queue
useful for memory-intensive searching

Array-based implementations are inflexible but space-efficient. If you know
how much memory you're going to need (or have available), you might as well
allocate all of it as compactly as possible.

Maybe I'm missing something, but I don't see any compelling reason to prefer
array-based min-max heaps or interval heaps over a pair of manually connected 
array heaps. Two separate arrays may even fit more easily into fragmented 
memory than a larger one.


User's guide

'a t: the type of DEP queues

make max x cmp: create a queue with a specified comparison function (boolean)
                and maximum number of elements
                * Odd values of max will be rounded up to the next even number.
                * The comparison function should return true when the better 
                  value is the first argument: cmp better worse = true.
                * You have to provide an initial value for Array.make to create
                  the heaps, but it won't actually be used in the queue.

insert q x: add a new item to the queue

best  q: the element at the top of the queue 
worst q: the element at the bottom of the queue

pop  q: remove and return the best element from the top of the queue
drop q: remove and return the worst element from the bottom of the queue

max q: the total capacity of the queue

size q: the number of elements currently in the queue

is_empty q: whether the queue doesn't have any elements in it (size q = 0)
is_full q: whether the queue is completely filled (size q = max q)

clear q: delete all elements from the queue (takes O(1) time)