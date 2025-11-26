let rec fix f x = f (fix f) x

type 'a rec_t = R of ('a rec_t -> 'a)

(* 1 - recurent types *)
let fix_rect f x = 
  let apply (R g) = fun x -> g (R g) x in
  let fixing g = f (apply g) in
  fixing (R fixing) x

(* 2 - mutowalny stan *)

let fix_ref f x = 
  let mem = ref (fun _ -> failwith "aaa") in
  mem := f (fun x -> !mem x);  (* użycie fun x -> !mem x zamiast !mem jest kluczowe bo tworzymy closure dla tej aktualnej mem*)
  !mem x
(* dalej dziala troche jak magia*)

let fib_m fib n = 
  if n <= 1 then n
  else fib (n-1) + fib (n-2)

let fib = fix_rect fib_m
let fib2 = fix_ref fib_m
