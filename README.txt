
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

make max x cmp: create a queue with a specified comparison function (boolean)
                and maximum number of elements. Odd values will be rounded up
                to the next even number. You have to provide an initial value
                for Array.make to create the heaps, but it won't actually be
                used.

insert q x: add a new item to the queue.

pop q: remove and return the best element from the top of the queue.

drop q: remove and return the worst element from the bottom of the queue.
