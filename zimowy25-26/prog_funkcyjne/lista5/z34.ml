(*leniwe drzewa*)
type 'a t = unit -> 'a ltree  
and 'a ltree = 
| LLeaf
| LNode of 'a t * 'a * 'a t

let rec rational_tree (num1,den1 : int * int) (num2,den2 : int * int) : (int * int) ltree = 
  let mid =  (num1 + num2,den1 + den2) in
  LNode(
    (fun () -> rational_tree (num1, den1) mid), 
    mid,
    (fun () -> rational_tree mid (num2, den2))
    ) 
   
let rec to_stream t = 
  match t with
  | LLeaf -> Seq.Nil
  | LNode(l,x,r) -> Cons(x, Seq.interleave (fun () -> to_stream (l ())) (fun () -> to_stream (r ())))

let all_rationals = rational_tree (0,1) (1,0)

let rat_stream = to_stream all_rationals

let list_rat = List.of_seq (Seq.take 10 ( fun () -> rat_stream))