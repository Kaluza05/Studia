let product xs = 
  let rec it xs acc = 
    match xs with
    | [] -> acc
    | x::xs' -> it xs' (x*acc)
  in it xs 1;;

let compose f g = fun x -> f (g x);;

let  build_list n f =
  let rec it k = 
    if k = n then [] else f k ::it (k+1)
  in it 0;;

let k_th_row k n = 
  let rec it m =
    if m = n then [] else
    if m = k then 1::it (m+1) else 0::it(m+1)
  in it 0;;

let negatives n = build_list n (fun x-> -x-1);;
let reciprocals n = build_list n (fun x-> 1./.float_of_int (x+1));;
let evens n = build_list n (fun x-> 2*x);;
let identity_M n = build_list n (fun k -> (build_list n (fun i-> if i=k then 1 else 0)));;

let empty_set = (fun _ -> false);;
let singleton a = (fun x -> x = a);;
let in_set a (t : 'a ->bool) = t a;;
let union s t = (fun x -> s x || t x);;
let intersection s t = (fun x -> s x && t x)