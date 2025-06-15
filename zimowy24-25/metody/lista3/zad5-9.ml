let rec append xs ys = 
  match xs with
  | [] -> ys
  | x::xs' -> x :: append xs' ys;;

type 'a tree = Leaf | Node of 'a tree * 'a * 'a tree;;
let t = Node( Node( Leaf,2,Leaf), 5, Node( Node ( Leaf , 6 , Leaf ) ,8, Node ( Leaf , 9 , Leaf ) ) );;

let rec insert_bst a tree =
  match tree with
  | Leaf -> Node(Leaf,a,Leaf)
  | Node (l,v,r) -> if a = v then tree
  else if a >v then Node(l,v,insert_bst a r) else Node(insert_bst a l,v,r);;

  
let rec fold_tree f t a= 
  match t with 
  | Leaf -> a
  | Node (l,v,r) -> f (fold_tree f l a)  v (fold_tree f r a);;

let tree_product t = fold_tree (fun l v r -> l*v*r) t 1;;
let tree_height t = fold_tree (fun l _ r-> 1+max l r) t 0;;
let tree_flip t = fold_tree (fun l v r -> Node(r,v,l)) t Leaf;;
let tree_span t = fold_tree (fun l v r ->
   let a = match l with
   | None -> v
   | Some (c,d) -> c
   and b = match r with
   | None -> v
   | Some (c,d) -> d
    in  Some (a,b))
    t None;;

let tree_span' t = fold_tree (fun l v r ->
  let a = match l with
  | (0,0) -> v
  | (c,d) -> c
  and b = match r with
  | (0,0) -> v
  | (c,d) -> d
   in  (a,b))
   t (0,0);;
let flatten t = fold_tree (fun l v r -> append (append l [v]) r) t [];;

let flatten_better t = 
  let rec flat_append t xs =
    match t with
    | Leaf -> xs
    | Node (l,v,r) -> flat_append l (v::(flat_append r xs))
in flat_append t [];;

let rec insert_bst_dup a t = 
  match t with
  | Leaf -> Node(Leaf,a,Leaf)
  | Node (l,v,r) -> if a = v then Node(l,v,Node(Leaf,a,r)) else
    if a > v then Node(l,v,insert_bst_dup a r) else Node(insert_bst_dup a l,v,r);; 

let tree_sort xs =
  let rec it xs t = 
    match xs with
    | [] -> t
    | x::xs' -> it xs' (insert_bst_dup x t)
  in flatten_better (it xs Leaf);;

let rec delete a t = 
  match t with
  | Leaf -> Leaf
  | Node (l,v,r) -> if a = v then
    if l = Leaf then r else if r = Leaf then l else
    let rec find_min t = 
      match t with
      | Leaf -> l
      | Node (l,v,r) -> Node(find_min l,v,r)
    in find_min r
    else if a > v then Node(l , v , delete a r) else Node( delete a l , v , r );;