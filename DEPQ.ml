(*
 * array-based double-ended priority queue (DEP queue)
 * heap implementation based on Cormen et. al., Introduction to Algorithms
 * copyright (c) 2021 Daniel S. Bensen
 *)


let is_even n = ( n  = 2*(n/2))
let is_odd  n = (n-1 = 2*(n/2))

exception Overflow
exception Underflow

let underflow () = raise Underflow
let  overflow () = raise Overflow

let[@inline] n_parent n = n/2    
let[@inline] n_left   n = 2*n    
let[@inline] n_right  n = 2*n + 1

let[@inline]         has_two_children          n size      = (n <= n_parent (size-1))                     
let[@inline]    is_leaf_with_opposite_leaf     n size      = (n > n_parent size)                  
let[@inline] is_last_parent_with_2_left_heaps  n size diff = (diff=0 && is_even size && n = n_parent size)  
let[@inline]          is_only_child            n size      = (is_even size && n = size)                     
let[@inline] other_heap_has_extra_left_child     size diff = (diff=1 && is_odd size)
let[@inline] last_parent_other_has_extra_right n size diff = (diff=1 && is_even size && n = n_parent size)


module Heap = struct

  type 'a t = { array: 'a array; mutable size: int; beats: 'a -> 'a -> bool }

  let max h = Array.length h.array

  let clear  h = h.size <- 0
  let shrink h = h.size <- h.size - 1; h.size
  let grow   h = h.size <- h.size + 1; h.size

  let ( .:()   ) h n   = h.array.(n-1)
  let ( .:()<- ) h n x = h.array.(n-1) <- x

  let  top h = if h.size > 0 then      h.:(1) else underflow ()
  let otop h = if h.size > 0 then Some h.:(1) else None

  let swap h1 n1 h2 n2 =
    let temp = h1.:(n1) in
    h1.:(n1)<-  h2.:(n2);
    h2.:(n2)<- temp

  let rec heapify_n h n =
    let nl = n_left  n
    and nr = n_right n
    in let nbig = if nl <= h.size && h.beats h.:(nl) h.:( n )  then nl else n
    in let nbig = if nr <= h.size && h.beats h.:(nr) h.:(nbig) then nr else nbig
    in
    if nbig = n then n
    else (swap h n h nbig; heapify_n h nbig)

  let heapify h = heapify_n h 1

  let build h = for n = h.size/2 downto 1 do ignore (heapify_n h n) done; h

  let make max x beats =       { array = Array.make max x; size =  0;  beats }
  let init max x beats = build { array = Array.init max x; size = max; beats }

  let pop h =
    if h.size = 0 then invalid_arg "pop: empty heap"
    else
    let x = top h in
    let n = shrink h in
    if n=0 then (x,0)
    else let () = h.:(1)<- h.:(n+1)
         in (x, heapify h)

  let float_n h n x =
    let rec do_n n =
      if not (n > 1 && h.beats x h.:(n_parent n))
      then (h.:(n)<- x; n)
      else (h.:(n)<- h.:(n_parent n);
            do_n (n_parent n))
    in do_n n

  let insertn h x =
    if h.size < max h then float_n h (grow h) x
    else invalid_arg "insert: heap is full"

  let insert h x = ignore (insertn h x)

  let floatify h n = heapify_n h (float_n h n h.:(n))

  let diff a b = a.size - b.size

  let hop src dest =
    let n = shrink src in
    insert dest src.:(n+1)

end

module H = Heap

let ( .:()   ) = H.( .:()   )

type 'a t = { hi: 'a H.t; lo: 'a H.t }

let[@inline] max  q = 2 * (Array.length q.lo.H.array)
let[@inline] size q = q.lo.H.size + q.hi.H.size

let[@inline] is_empty q = size q = 0
let[@inline] is_full  q = size q = max q

let[@inline] hdiff q = q.hi.H.size - q.lo.H.size

let[@inline] hhi q = q.hi
let[@inline] hlo q = q.lo

let tob fh q =           (* top or bottom :) *)
  let size = size q
  in   if size > 1 then (fh q).H.:(1)
  else if size = 0 then underflow()
  else if hdiff q = 1
  then q.hi.H.:(1)
  else q.lo.H.:(1)

let otob fh q =
  let size = size q
  in   if size > 1 then Some (fh q).H.:(1)
  else if size = 0 then None
  else if hdiff q = 1
  then Some q.hi.H.:(1)
  else Some q.lo.H.:(1)

let  top q =  tob hhi q
let otop q = otob hhi q

let  bottom q =  tob hlo q
let obottom q = otob hlo q

let clear q =
  H.clear q.hi;
  H.clear q.lo

let make max x beats =
  let n = (max+1)/2 in
  { hi = H.make n x beats;
    lo = H.make n x (Fun.flip beats) }

let insert q xnew =
  if is_full q then overflow()
  else
  let insert1 a b =
    if not (a.H.beats xnew a.:(1))
    then H.insert b xnew
    else (H.hop a b; H.insert a xnew)
  in
  match (q.lo.H.size, q.hi.H.size) with
  | 0,0 -> H.insert q.hi xnew
  | 0,1 -> insert1 q.hi q.lo
  | 1,0 -> insert1 q.lo q.hi
  | slo,shi when slo=shi ->
       if q.hi.H.beats xnew q.hi.:(n_parent (slo+1))
       then H.insert q.hi xnew
       else H.insert q.lo xnew
  | slo,shi ->
       let (a,b) = if      shi-slo=1 then (q.hi,q.lo)
                   else if slo-shi=1 then (q.lo,q.hi) else invalid_arg "bad hdiff"
       in let n = H.shrink a
       in let xmid = a.:(n+1)
       in 
       if a.H.beats xnew xmid
       then (H.insert a xnew; H.insert b xmid)
       else (H.insert a xmid; H.insert b xnew)

let oinsert q xnew =
  if is_full q then None
  else Some (insert q xnew)

let pop_n_swap h1 h2 =
  let (x,n1) = H.pop h1 in
  let rec
      do_n a n b =
    if not (a.H.beats b.:(n) a.:(n))
    then x
    else (H.swap a n b n; do_swap b (H.floatify b n) a)
  and
      do_2 a na b nb =
    if not (a.H.beats b.:(nb) a.:(na))
    then x
    else
    let () = H.swap a na b nb in
    let nbnew = H.floatify b nb in
    if nbnew = nb then x
    else (H.swap a na b nb; do_swap b nbnew a)
  and
      do_swap a na b =
    let size = a.H.size
    and diff = H.diff b a
    in 
    if             has_two_children           na size      then x
    else if    is_leaf_with_opposite_leaf     na size      then do_n a na b
    else if is_last_parent_with_2_left_heaps  na size diff then x
    else if          is_only_child            na size      then do_n a na b
    else if other_heap_has_extra_left_child      size diff then do_2 a na b (n_left na)
    else if last_parent_other_has_extra_right na size diff then do_2 a na b (n_right na)
    else
      invalid_arg (Printf.sprintf "na=%d, size=%d, diff=%d" na size diff)
  in
  do_swap h1 n1 h2

let pop1 q =
  if hdiff q = -1              (* if there's an extra element in the other heap, *)
  then H.hop q.lo q.hi;        (*        move it to the heap being popped        *)
  pop_n_swap q.hi q.lo  

let drop1 q =
  if hdiff q = 1 then H.hop q.hi q.lo;
  pop_n_swap q.lo q.hi

let   pop q = if size q > 0 then        pop1 q  else underflow()
let  drop q = if size q > 0 then       drop1 q  else underflow()
let  opop q = if size q > 0 then Some ( pop1 q) else None
let odrop q = if size q > 0 then Some (drop1 q) else None


module type Typeof_Element = sig
  type t
  val beats: t -> t -> bool
end

module Make (E: Typeof_Element) = struct

  type element = E.t

  module Heap = struct

    type t = { array: element array; mutable size: int; beats: element -> element -> bool }

    let make max x beats = { array = Array.make max x; size = 0; beats }

    let max h = Array.length h.array

    let clear  h = h.size <- 0
    let shrink h = h.size <- h.size - 1; h.size
    let grow   h = h.size <- h.size + 1; h.size

    let ( .:()   ) h n   = h.array.(n-1)
    let ( .:()<- ) h n x = h.array.(n-1) <- x

    let top h = if h.size > 0 then h.:(1) else underflow ()

    let swap h1 n1 h2 n2 =
      let temp = h1.:(n1) in
      h1.:(n1)<-  h2.:(n2);
      h2.:(n2)<- temp

    let rec heapify_n h n =
      let nl = n_left  n
      and nr = n_right n
      in let nbig = if nl <= h.size && h.beats h.:(nl) h.:( n )  then nl else n
      in let nbig = if nr <= h.size && h.beats h.:(nr) h.:(nbig) then nr else nbig
      in
      if nbig = n then n
      else (swap h n h nbig; heapify_n h nbig)

    let heapify h = heapify_n h 1

    (* let build h = for n = h.size/2 downto 1 do ignore (heapify_n h n) done; h *)

    let pop h =
      if h.size = 0 then invalid_arg "pop: empty heap"
      else
      let x = top h in
      let n = shrink h in
      if n=0 then (x,0)
      else let () = h.:(1)<- h.:(n+1)
           in (x, heapify h)

    let float_n h n x =
      let rec do_n n =
        if not (n > 1 && h.beats x h.:(n_parent n))
        then (h.:(n)<- x; n)
        else (h.:(n)<- h.:(n_parent n);
              do_n (n_parent n))
      in do_n n

    let insertn h x =
      if h.size < max h then float_n h (grow h) x
      else invalid_arg "insert: heap is full"

    let insert h x = ignore (insertn h x)

    let floatify h n = heapify_n h (float_n h n h.:(n))

    let diff a b = a.size - b.size

    let hop src dest =
      let n = shrink src in
      insert dest src.:(n+1)

  end

  module H = Heap

  let ( .:()   ) = H.( .:()   )

  type t = { hi: H.t; lo: H.t }

let[@inline] max  q = 2 * (Array.length q.lo.H.array)
let[@inline] size q = q.lo.H.size + q.hi.H.size

let[@inline] is_empty q = size q = 0
let[@inline] is_full  q = size q = max q

let[@inline] hdiff q = q.hi.H.size - q.lo.H.size

let[@inline] hhi q = q.hi
let[@inline] hlo q = q.lo

let tob fh q =           (* top or bottom :) *)
  let size = size q
  in   if size > 1 then (fh q).H.:(1)
  else if size = 0 then underflow()
  else if hdiff q = 1
  then q.hi.H.:(1)
  else q.lo.H.:(1)

let otob fh q =
  let size = size q
  in   if size > 1 then Some (fh q).H.:(1)
  else if size = 0 then None
  else if hdiff q = 1
  then Some q.hi.H.:(1)
  else Some q.lo.H.:(1)

let  top q =  tob hhi q
let otop q = otob hhi q

let  bottom q =  tob hlo q
let obottom q = otob hlo q

  let hdiff q = q.hi.H.size - q.lo.H.size

  let clear q =
    H.clear q.hi;
    H.clear q.lo

  let make max x =
    let n = (max+1)/2 in
    { hi = H.make n x E.beats;
      lo = H.make n x (Fun.flip E.beats) }

  let insert q xnew =
    if is_full q then overflow()
    else
    let insert1 a b =
      if not (a.H.beats xnew a.:(1))
      then H.insert b xnew
      else (H.hop a b; H.insert a xnew)
    in
    match (q.lo.H.size, q.hi.H.size) with
    | 0,0 -> H.insert q.hi xnew
    | 0,1 -> insert1 q.hi q.lo
    | 1,0 -> insert1 q.lo q.hi
    | slo,shi when slo=shi ->
         if q.hi.H.beats xnew q.hi.:(n_parent (slo+1))
         then H.insert q.hi xnew
         else H.insert q.lo xnew
    | slo,shi ->
         let (a,b) = if      shi-slo=1 then (q.hi,q.lo)
                     else if slo-shi=1 then (q.lo,q.hi) else invalid_arg "bad hdiff"
         in let n = H.shrink a
         in let xmid = a.:(n+1)
         in 
         if a.H.beats xnew xmid
         then (H.insert a xnew; H.insert b xmid)
         else (H.insert a xmid; H.insert b xnew)

  let oinsert q xnew =
    if is_full q then None
    else Some (insert q xnew)

  let pop_n_swap h1 h2 =
    let (x,n1) = H.pop h1 in
    let rec
        do_n a n b =
      if not (a.H.beats b.:(n) a.:(n))
      then x
      else (H.swap a n b n; do_swap b (H.floatify b n) a)
    and
        do_2 a na b nb =
      if not (a.H.beats b.:(nb) a.:(na))
      then x
      else
      let () = H.swap a na b nb in
      let nbnew = H.floatify b nb in
      if nbnew = nb then x
      else (H.swap a na b nb; do_swap b nbnew a)
    and
        do_swap a na b =
      let size = a.H.size
      and diff = H.diff b a
      in 
      if             has_two_children           na size      then x
      else if    is_leaf_with_opposite_leaf     na size      then do_n a na b
      else if is_last_parent_with_2_left_heaps  na size diff then x
      else if          is_only_child            na size      then do_n a na b
      else if other_heap_has_extra_left_child      size diff then do_2 a na b (n_left na)
      else if last_parent_other_has_extra_right na size diff then do_2 a na b (n_right na)
      else
        invalid_arg (Printf.sprintf "na=%d, size=%d, diff=%d" na size diff)
    in
    do_swap h1 n1 h2

  let pop1 q =
    if hdiff q = -1              (* if there's an extra element in the other heap, *)
    then H.hop q.lo q.hi;        (*        move it to the heap being popped        *)
    pop_n_swap q.hi q.lo  

  let drop1 q =
    if hdiff q = 1 then H.hop q.hi q.lo;
    pop_n_swap q.lo q.hi

  let   pop q = if size q > 0 then        pop1 q  else underflow()
  let  drop q = if size q > 0 then       drop1 q  else underflow()
  let  opop q = if size q > 0 then Some ( pop1 q) else None
  let odrop q = if size q > 0 then Some (drop1 q) else None

end


(********************* testing ****************************)

let rec is_partial h n =
  let open Heap in
  let nl = n_left  n
  and nr = n_right n
  in (nl > h.size || (not (h.beats h.:(nl) h.:(n))) && is_partial h nl)
  && (nr > h.size || (not (h.beats h.:(nr) h.:(n))) && is_partial h nr)

let is_heap h = is_partial h 1

let   fails  f x = try let _ = f x in false with _ -> true
let succeeds f x = try let _ = f x in true  with _ -> false

let test_clear h =
  let open Heap in
  let () = clear h in
  h.size = 0 && fails top h && fails pop h

let test_heap () =
    let open Heap in
    let h = init 20 Fun.id (>)
    in is_heap h
    && fails (insert h) 0
    && top h = 19
    && otop h = Some 19
    && fst (pop h) = 19
    && top h = 18
    && let _ = insert h 42
       in test_clear h
    && succeeds (fun () -> for n = 1 to 20 do ignore (insert h n) done; true) ()
    && succeeds (fun () -> for n = 20 downto 1 do if fst (pop h) <> n then invalid_arg "popped wrong element" done; true) ()

let _ = test_heap() || failwith "failed Heap test"
