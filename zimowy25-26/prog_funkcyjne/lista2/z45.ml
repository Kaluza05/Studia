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

  (*tu dodajemy z przodu dlatego zs trzeba odwrocić*)
let merge2 cmp xs ys = 
  let rec help xs ys zs = 
    match xs,ys with
    | [],ys -> List.rev_append zs ys
    | xs,[] -> List.rev_append zs xs
    | x :: xs', y :: ys' -> if cmp x y then help xs' ys (x :: zs) else help xs ys' (y :: zs) 
    
  in help xs ys []

let rec mergesort cmp xs = 
  match xs with
  | [] | [_] -> xs
  | _ -> let ys,zs = halve xs in merge cmp (mergesort cmp ys) (mergesort cmp zs)

(* inaczej merge zrobić, bez odwracania listy?*)
let xs = List.init 10000 (fun x -> x + 1);;
let ys = List.init 10000 (fun x -> -x - 1);;

(*merge2 działa o wiele szybciej or merge*)

let rec print_list xs = 
  match xs with
  | [] -> ""
  | x :: xs -> string_of_int x ^ " : " ^ print_list xs 

let merge xs ys = 
  let rec it xs ys f =  (*f to funkcja ktora doczepa element z przodu*)
    match xs,ys with
    | xs, [] -> f xs
    | [], ys -> f ys
    | x :: xs', y :: ys' -> if x < y then 
      it xs' ys (fun l -> f  (x :: l)) else 
      it xs ys' (fun l -> f  (y :: l))

in it xs ys (fun x -> x)

(* podobne do nieogonowego, tylko zamiast dodawać z przodu dodajemy x do akumulatora w podobny sporsob, gdzie 
fun l -> f (x :: l) , l będzie odpowiadał wynikowi it xs' ys *)

let rec slow_merge xs ys = 
  match xs, ys with
  | xs, [] -> xs
  | [], ys -> ys
  | x :: xs', y :: ys' -> if x < y then x :: slow_merge xs' ys else y :: slow_merge xs ys'


(*mergesort z zamienianiem operatora porownania*)