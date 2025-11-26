module type RandomMonad = sig
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val random : int t
end

module Shuffle(R : RandomMonad) : sig
  val shuffle : 'a list -> 'a list R.t
end = struct
  let rec shuffle xs = 
    (* doesnt care about order of the rest elements *)
    let n_th_rest xs n =
      let rec it xs' ys i = 
        match xs' with
        | [] -> failwith "shouldn't happen"
        | x :: xs' -> if i = n then x, ys @ xs' else it xs' (x :: ys) (i + 1) 
      in it xs [] 0
    in
    
    let (let*) = R.bind in
    match xs with
    | [] -> R.return []
    | _ ->
      let* i = R.random in
      let i' = i mod (List.length xs) in 
      let x,rest = n_th_rest xs i' in 
      let* rest = shuffle rest in R.return (x :: rest)
end

 
module RS : RandomMonad with type 'a t = int -> 'a * int = struct
  type 'a t = int -> 'a * int

  let return x = fun i -> x, i
  let bind a f = fun i -> 
    let x, i = a i in f x i
  let random = fun a_i ->
  let m = 2147483647 in
  let a = a_i in
  let hi = a / 127773 in
  let lo = a mod 127773 in
  let b = 16807 * lo - 2836 * hi in
  let a_next =
    if b > 0 then b else b + m
  in
  (a_next, a_next)
end

module ShuffleList = Shuffle(RS)

let random_list = fst(ShuffleList.shuffle [1;2;3;4;5;6;7;8] 1);;