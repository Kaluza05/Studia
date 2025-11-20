(* gotta understand it better cuz wtf*)

(*fix jest 'a -> 'b -> 'c
f ('b -> 'c) -> 'b -> 'c

zatem 'a = tamto

f : ('a -> 'b) -> 'a -> 'b
x : 'a
fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
*)
exception LimitError

let rec fix f x = f (fix f) x

let rec fix_with_limit n f x = 
  if n = 0 then raise LimitError
  else f (fix_with_limit (n-1) f) x

let fix_with_memo (f : ('a -> 'b) -> 'a -> 'b) =
  let memo = Hashtbl.create 100 in
  let rec fix f x =  
    if Hashtbl.mem memo x then Hashtbl.find memo x else
    let fix_val = f (fix f) x in 
    Hashtbl.add memo x fix_val;
    fix_val
    
  in fix f

let fib_f fib n =
if n <= 1 then n
else fib (n-1) + fib (n-2)

let fib = fix fib_f

let fib2 = fix_with_limit 10 fib_f

let fib3 = fix_with_memo fib_f


(* 
to nie działa z jakiegoś powodu
*)
(*
let fix_with_memo (type a ) (type b ) (f : (a -> b) -> a -> b) =
  let memo = Hashtbl.create 100 in
  let rec fix f x =  
    if Hashtbl.mem memo x then Hashtbl.find memo x else
    Hashtbl.add memo x (f (fix f) x);
    (f (fix f) x)
    
  in fun (x : a) ->  fix f x

*)
