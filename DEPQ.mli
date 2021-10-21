(*
 * array-based double-ended priority queue (DEP queue)
 * heap implementation based on Cormen et. al., Introduction to Algorithms
 * other code copyright (c) 2021 Daniel S. Bensen
 *)

type 'a t

val make: int -> 'a -> ('a -> 'a -> bool) -> 'a t

val max:  'a t -> int
val size: 'a t -> int

val insert q x: 'a t -> 'a -> ()

val best  q: 'a t -> 'a 
val worst q: 'a t -> 'a

val pop:  'a t -> 'a
val drop: 'a t -> 'a

val is_empty: 'a t -> bool
val is_full:  'a t -> bool

val clear: 'a t -> ()