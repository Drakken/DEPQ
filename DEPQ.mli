(*
 * array-based double-ended priority queue (DEP queue)
 * heap implementation based on Cormen et. al., Introduction to Algorithms
 * copyright (c) 2021 Daniel S. Bensen
 *)

exception Overflow
exception Underflow

type 'a t

val make: int -> 'a -> ('a -> 'a -> bool) -> 'a t

val max:  'a t -> int
val size: 'a t -> int

val  insert: 'a t -> 'a -> unit
val oinsert: 'a t -> 'a -> unit option

val top:   'a t -> 'a
val pop:   'a t -> 'a
val drop:  'a t -> 'a
val bottom:'a t -> 'a

val otop:   'a t -> 'a option
val opop:   'a t -> 'a option
val odrop:  'a t -> 'a option
val obottom:'a t -> 'a option

val is_empty: 'a t -> bool
val is_full:  'a t -> bool

val clear: 'a t -> unit


module type Typeof_Element = sig
  type t
  val beats: t -> t -> bool
end

module Make (E: Typeof_Element): sig

  type element = E.t
  type t

  val make : int -> element -> t

  val max:   t -> int
  val size : t -> int

  val  insert: t -> element -> unit
  val oinsert: t -> element -> unit option

  val top  : t -> element
  val pop  : t -> element
  val drop : t -> element
  val bottom:t -> element

  val otop  : t -> element option
  val opop  : t -> element option
  val odrop : t -> element option
  val obottom:t -> element option

  val is_empty: t -> bool
  val is_full:  t -> bool

  val clear: t -> unit

end
