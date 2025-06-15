let rec mem x xs = 
  match xs with
  | [] -> false
  | y :: xs' -> if x = y then true else mem x xs';;

let maximum xs = 
  let rec it xs max_curr = 
    match xs with
    | [] -> max_curr
    | x :: xs' -> if x > max_curr then it xs' x else it xs' max_curr
  in it xs neg_infinity;;


let rec suffixes xs = 
  match xs with
  | [] -> [[]]
  | _ :: xs' -> xs :: suffixes xs';;

