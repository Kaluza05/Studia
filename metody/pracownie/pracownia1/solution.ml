type 'a tree = Leaf | Node of 'a tree * 'a * 'a tree
type 'a sgtree = { tree : 'a tree; size : int; max_size : int }

let alpha_num = 3
let alpha_denom = 4

let divide (list : 'a list) : 'a list * 'a * 'a list =
  let len = List.length list in
  if len = 0 then failwith "empty list"
  else
    let rec it l l1 k =
      if k = len / 2 then (List.rev l1, List.hd l, List.tl l)
      else
        match l with
        | x :: xs' -> it xs' (x :: l1) (k + 1)
        | [] -> failwith "impossible case"
    in
    it list [] 0

let rec size (t : 'a tree) : int =
  match t with Leaf -> 0 | Node (l, _, r) -> size l + size r + 1

let alpha_height (n : int) : int =
  Float.log (float_of_int n)
  /. Float.log (float_of_int alpha_denom /. float_of_int alpha_num)
  |> Float.floor |> int_of_float

let alpha_weight_balance (s_child1 : int) (s_child2 : int): bool =
  let s_tree = s_child1 + s_child2 + 1 in 
  alpha_denom * s_child1 <= alpha_num * s_tree && 
  alpha_denom * s_child2 <= alpha_num * s_tree
  
let rebuild_balanced (t : 'a tree) : 'a tree =
  let rec rebuild (list : 'a list) : 'a tree =
    if list = [] then Leaf
    else
      let l, v, r = divide list in
      Node (rebuild l, v, rebuild r)
  and tree_to_list (tree : 'a tree) : 'a list =
    let rec it tree acc =
      match tree with Leaf -> acc | Node (l, v, r) -> it l (v :: it r acc) 
    in 
    it tree []
  in
  t |> tree_to_list |> rebuild

let empty : 'a sgtree = { tree = Leaf; size = 0; max_size = 0 }

let find (x : 'a) (sgt : 'a sgtree) : bool =
  let rec find_val (tree : 'a tree) : bool =
    match tree with
    | Leaf -> false
    | Node (l, v, r) ->
        if x = v then true else if x > v then find_val r else find_val l
  in
  find_val sgt.tree

let insert (x : 'a) (sgt : 'a sgtree) : 'a sgtree =
  let rec it (tree : 'a tree) (depth : int) (rebuild : int option) : 'a tree * int option =
    match tree with
    | Leaf ->
        ( Node (Leaf, x, Leaf),
          if depth > alpha_height sgt.size then Some 1 else None )
    | Node (l, v, r) ->
        if x = v then failwith "element already in tree"
        else
          let (other, current) = if x > v then (l,r) else (r,l)
          in
          let child_node = (it current (depth + 1) rebuild)
          in
          let curr_node =
            if x > v then Node (l, v, fst child_node)
            else Node (fst child_node, v, r)
          in
          if snd child_node = None then (curr_node, None) else 
            let ch1, ch2 = Option.get (snd child_node), size other in 
            if alpha_weight_balance ch1 ch2 then
              (curr_node, Some (ch1 + ch2 + 1))
            else (rebuild_balanced curr_node, None)
  in
  {
    tree = fst (it sgt.tree 0 None);
    size = sgt.size + 1;
    max_size = max sgt.max_size (sgt.size + 1);
  }

let remove (x : 'a) (sgt : 'a sgtree) : 'a sgtree =
  let rebuild = alpha_denom * (sgt.size - 1) < alpha_num * sgt.max_size in
  let rec del (tree : 'a tree) : 'a tree =
    match tree with
    | Leaf -> failwith "value not in tree"
    | Node (l, v, r) ->
        if x = v then
          if l = Leaf then r else if r = Leaf then l
          else
            let rec insert_left (t : 'a tree) : 'a tree =
              match t with
              | Leaf -> l
              | Node (l, v, r) -> Node (insert_left l, v, r)
            in
            insert_left r
        else if x > v then Node (l, v, del r)
        else Node (del l, v, r)
  in
  let tree = del sgt.tree in
  let new_tree = if rebuild then rebuild_balanced tree else tree
  and new_max_size = if rebuild then sgt.size - 1 else sgt.max_size in
  { tree = new_tree; size = sgt.size - 1; max_size = new_max_size }