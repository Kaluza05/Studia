(*z1*)

let rec length xs = List.fold_left (fun acc _ -> 1 + acc) 0 xs
let rec rev xs = List.fold_left (fun acc x -> x :: acc) [] xs
let rec map f xs = List.fold_right (fun x acc -> f x :: acc) xs []
let rec append xs ys = List.fold_right (fun x acc -> x :: acc) xs ys
let rec rev_append xs ys = List.fold_left (fun acc x -> x :: acc) ys xs
let rec filter f xs = List.fold_right (fun x acc -> if f x then x :: acc else acc) xs []
let rec rev_map f xs = List.fold_left (fun acc x -> f x :: acc) [] xs

(*przypomnienie foldów*)
let rec fold_left f acc xs = 
  match xs with
  | [] -> acc
  | x :: xs -> fold_left f (f acc x) xs

let rec fold_right f xs start = 
  match xs with
  | [] -> start
  | x :: xs -> f x (fold_right f xs start)

(*z2*)
let sublists xs = 
  let rec gen xs generated = 
    match xs with
    | [] -> generated
    | x :: xs -> gen xs (generated @ (List.map (fun ls -> x :: ls) generated)) (*wszystkie z x + wszystkie bez x*)
  in gen xs [[]]

let sublists2 xs = List.fold_left (fun generated x -> generated @ (List.map (fun ls -> x :: ls) generated) ) [[]] xs

(*z3*)
let rec suffixes xs = 
  match xs with
  | [] -> [[]]
  | _ :: xs' -> xs :: suffixes xs' 

let suffixes_tail xs =
  let rec help xs generated = 
    match xs with
    | [] -> generated
    | x :: xs' -> help xs' (xs :: generated)
  in [] :: help xs []

let suffixes2 xs = List.fold_right (fun x acc -> (x :: List.hd acc) :: acc) xs [[]]

let rec prefixes xs =
  match xs with 
  | [] -> [[]]
  | x :: xs -> [] :: List.map (fun ls -> x :: ls) (prefixes xs)

let prefixes2 xs = List.fold_left (fun acc x -> ((List.hd acc) @ [x]) :: acc) [[]] xs

let prefixes3 xs = List.fold_right (fun x acc -> [] :: List.map (fun ls -> x :: ls) acc) xs [[]]


(* help xs before makes a list of permutations starting from x and appending it to other permutations,
selection*)
let rec perms_select xs = 
  let rec help xs before = 
    match xs with
    | [] -> failwith "shouldnt be empty"
    | [x] -> List.map (fun ls -> x :: ls) (perms_select before)
    | x :: y :: xs' -> (List.map (fun ls -> x :: ls) 
      (perms_select (y :: before @ xs')))  (*permutacje gdzie x jest z przodu (x dołączony z przodu do innych permutacji)*)
      @ help (y :: xs') (x :: before) (*rekurencyjne wywołanie z dodaniem x do elementów przed*)
  in match xs with
    | [] -> [[]]
    | _ -> help xs []



let thrd (_,_,a) = a

let insert_x xs x = thrd 
  (List.fold_left 
  (fun (b,after, gen) y -> (b @ [y], List.tl after, (b @ x :: after) :: gen)) ([],xs,[xs @ [x]]) xs)

let insert2 xs x = 
  let rec it before xs acc = 
    match xs with
    | [] -> acc
    | y :: xs' -> it (before @ [y]) (xs') ((before @ x :: xs) :: acc)
    in it [] xs [xs @ [x]]

let perm_insert xs =
  let rec it xs gen =  
  match xs with
  | [] -> gen
  | x :: xs' -> it xs' (List.fold_left (fun acc ls -> (insert2 ls x) @ acc) [] gen)
  in it xs [[]]


let xs = [1;2;3]