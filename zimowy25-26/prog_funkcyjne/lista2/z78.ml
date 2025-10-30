module Heap = struct
  type 'a heap = Leaf | Node of int * 'a heap * 'a * 'a heap

  let rank = function Leaf -> 0 | Node(r,_,_,_) -> r
  
  let make_node h1 h2 x = if rank h1 > rank h2 then Node(rank h2 + 1 ,h1, x, h2) else Node (rank h1 + 1, h2, x, h1)

  let rec merge h1 h2 = 
    match h1,h2 with
    | Leaf, Leaf -> Leaf
    | Leaf, _ -> h2
    | _, Leaf -> h1
    | Node(r1,h1l,x,h1r), Node(r2,h2l,y,h2r) -> 
      if x <= y then make_node h1l (merge h1r h2) x
      else          make_node h2l (merge h2r h1) y

  let empty = Leaf
  let insert x h = merge (make_node Leaf Leaf x) h

  let pop h = 
    match h with
    | Leaf -> Leaf
    | Node(_,h1,_,h2) -> merge h1 h2

  let find_min h = 
    match h with
    | Leaf -> None
    | Node(_,_,x,_) -> Some x
end

open Heap

let rec print_heap h = 
    match h with
    | Leaf -> "Leaf"
    | Node(_,hl,x,hr) -> "Node( " ^ print_heap hl ^ ", "^ string_of_int x ^ ", " ^ print_heap hr ^ ")"
let h1 = empty |> insert 5 |> insert 3 |> insert 7 |> insert 1
let h2 = pop h1
let h3 = h2 |> insert 4 |> insert 1
let h4 = empty |> insert 7 |> insert 8