let for_all p ls = 
  let exception ForAllFold in
  try
    List.fold_left (fun _ x -> if p x then true else raise ForAllFold) true ls 
  with 
    | ForAllFold -> false

let mult_list ls = 
  let exception Zero in
  try
    List.fold_left (fun acc x -> if x <> 0 then x * acc else raise Zero) 1 ls 
  with 
    | Zero -> 0

let sorted ls = 
  let exception NotSorted in
  match 
    try
      List.fold_left (fun acc x -> 
        match acc with
        | None -> Some(x)
        | Some(y) when y < x -> Some(x)
        | _ -> raise NotSorted) 
        None ls 
    with 
      | NotSorted -> None
  with
  | None   -> false
  | Some _ -> true

(*
zeby sprawdzic fold leftem, ze [1;2;3] jest posortowane rosnaco musimy sprawdzic ze acc < x, bo acc jest z przodu
*)

let ls = [1;2;3;4]
let ls2 = [1;3;2;4]

let ls3 = [2;2;2;2]
let ls4 = [4;3;2;1]
let p = fun x -> x = 2

let long_list = List.init (1000*1000) (fun i -> 2)


let long_list = 0 :: List.init (10000000) (fun i -> 2)