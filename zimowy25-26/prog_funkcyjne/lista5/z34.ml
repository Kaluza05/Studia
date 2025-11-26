(*leniwe drzewa*)
type 'a ltree = unit -> 'a ltree_data  
and 'a ltree_data = 
| LLeaf
| LNode of 'a ltree * 'a * 'a ltree

let rec rational_tree (num1,den1 : int * int) (num2,den2 : int * int) : (int * int) ltree = 
  let mid =  (num1 + num2,den1 + den2) in
  fun () -> LNode(
    rational_tree (num1, den1) mid, 
    mid,
    rational_tree mid (num2, den2)
    ) 
   
let rec to_stream t = 
  match t () with
  | LLeaf -> Seq.Nil
  | LNode(l,x,r) -> Cons(x, Seq.interleave (fun () -> to_stream l) (fun () -> to_stream r))

let all_rationals = rational_tree (0,1) (1,0)

let rat_stream = to_stream all_rationals

let list_rat n = List.of_seq (Seq.take n ( fun () -> rat_stream))