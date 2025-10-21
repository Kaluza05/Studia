let halve xs = 
  let rec help xs ys zs = 
    match xs with
    | [] -> ys,zs
    | x :: xs -> help xs zs (x :: ys)
  in help xs [] []

let halve2 xs = 
  let rec help xs ys zs = 
    match xs with
    | [] -> ys,zs
    | [x] -> x :: ys, zs
    | x :: y :: xs' -> help xs' (x :: ys) (y :: zs)
  in help xs [] []

let merge cmp xs ys = 
  let rec help xs ys zs = 
    match xs,ys with
    | [],ys -> zs @ ys
    | xs,[] -> zs @ xs
    | x :: xs', y :: ys' -> if cmp x y then help xs' ys (zs @ [x]) else help xs ys' (zs @ [y]) 
  in help xs ys []

let merge2 xs ys = 
  let rec help xs ys zs = 
    match xs,ys with
    | [],ys -> List.rev_append zs ys
    | xs,[] -> List.rev_append zs xs
    | x :: xs', y :: ys' -> if x < y then help xs' ys (x :: zs) else help xs ys' (y :: zs) 
  in help xs ys []

let rec mergesort cmp xs = 
  match xs with
  | [] | [_] -> xs
  | x :: xs' -> let ys,zs = halve xs in merge cmp (mergesort cmp ys) (mergesort cmp zs)

(* inaczej merge zrobić, bez odwracania listy?*)