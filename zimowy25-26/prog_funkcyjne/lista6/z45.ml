let rec fold_left_cps  (f : 'acc -> 'a -> ('acc -> 'c) -> 'c) (acc : 'acc ) (ls : 'a list) (cont : 'acc -> 'c) : 'c =
  match ls with
  | [] -> cont acc
  | x :: xs -> f acc x (fun acc' -> fold_left_cps f acc' xs cont)

let fold_left (f : 'acc -> 'a -> 'acc) (acc : 'acc) (ls : 'a list) : 'acc = 
  (*always continue*)
  fold_left_cps (fun acc x cont -> cont (f acc x)) acc ls (fun acc -> acc)


let for_all p ls = fold_left_cps (fun acc x cont -> if p x then cont true else false) true ls (fun x -> x)
let slow_all p ls = List.fold_left (fun acc x -> p x && acc) true ls

let mult_list ls = fold_left_cps (fun acc x cont -> if x <> 0 then cont (x * acc) else 0) 1 ls (fun x -> x)
let slow_mult ls = List.fold_left (fun acc x -> x * acc) 1 ls

let sorted ls = 
  let module Sorted = struct
  type 'a is_sorted = No | Yes of 'a | Empty 
  end in
  let open Sorted in 
  match fold_left_cps (fun acc x cont -> 
    match acc with
    | Empty             -> cont (Yes(x))
    | Yes(y) when y < x -> cont (Yes(x))
    | Yes _ | No        -> No          ) Empty ls (fun x -> x)
  with
  | Yes _ -> true
  | Empty -> true
  | No    -> false



let long_list_bool = false :: List.init (10000000) (fun i -> true);;

let long_list_int = 0 :: List.init (10000000) (fun i -> 2)