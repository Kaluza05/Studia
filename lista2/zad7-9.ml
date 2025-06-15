let is_sorted xs = 
  if xs = [] then true else
  let rec it xs el = 
    match xs with
    | [] -> true
    | x :: xs' -> if x>= el then it xs' x else false
  in it xs (List.hd xs );;


let select xs = 
  let rec find_min ys curr_min =
    match ys with
    | [] -> curr_min
    | y::ys' -> if y<curr_min then find_min ys' y else find_min ys' curr_min
  and sel_rec ys el = 
    match ys with
    | [] -> []
    | y::ys' -> if y = el then ys' else y :: (sel_rec ys' el)
  in
  let smallest = (find_min xs max_int) in 
  (smallest,(sel_rec xs smallest));;

let rec select_sort xs =
  if xs = [] then [] else
  match select xs with
  | (a,rest) -> a :: select_sort rest;;


let split xs = 
  let rec split_t ys zs ms = 
    match ys with
    | [] -> (zs,ms)
    | y::ys' -> split_t ys' ms (y::zs)
  in
  split_t xs [] [] ;;

let rec append xs ys =
  match xs with
  | [] -> ys
  | x::xs -> x :: append xs ys;;
let  rec merge xs ys =
  match (xs,ys) with
  | ([],_) -> ys
  | (_,[]) -> xs
  | (x::xs',y::ys') -> if x < y then x :: (merge xs' ys) else y :: (merge xs ys');;

let  rec merge_sort xs = 
  match  xs with
  | [] -> []
  | x::[] -> [x]
  | _ ->
  let (half1,half2) = (split xs) in merge (merge_sort half1) (merge_sort half2);;