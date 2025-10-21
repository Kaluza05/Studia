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
      if x < y then Node(r1,merge h1l h2,x,h1r) 
      else          Node(r2,merge h2l h1,y,h2r) 

  let insert x h = merge (make_node Leaf Leaf x) h

  let pop h = 
    match h with
    | Leaf -> Leaf
    | Node(_,h1,_,h2) -> merge h1 h2
end
