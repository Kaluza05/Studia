(* z1 *)

(* fun x -> x ma typ 'a -> 'a*)

let f x = x
let f_inf (x : int) = x

let f1 f g x = f (g x)
let f2 x _ = x
let f3 x y = if x = y then x else y

(* z2 *)
let rec f x = f x


(* z3 *)

module STREAM = struct
  type 'a stream = int -> 'a

  let hd (s : 'a stream) : 'a = s 0
  let tl (s : 'a stream) : ('a stream)= fun i -> s (i + 1)

  let add (s : 'a stream) (c : int) = fun i -> (s i) + c

  let map (f : 'a -> 'a) (s : 'a stream) : ('a stream)= fun i -> f (s i)

  let map2 (f : 'a -> 'b -> 'c) (s1 : 'a stream) (s2 : 'b stream) : ('c stream) = 
      fun i -> f (s1 i) (s2 i)

  let replace (s : 'a stream) (n : int) (a : 'a) : ('a stream) =
    fun i -> if i = n then a else s i

  let take_every (s : 'a stream) (n : int) : ('a stream) = 
    fun i -> s (i * n)

  (* z4 *)
  let scan (f : 'a -> 'b -> 'a) (a : 'a) (s : 'b stream) : 'a stream = 
    let rec curry n = if n = 0 then f a (s 0) else f (curry (n - 1)) (s n) in fun i -> curry i

  (* z5 *)
  let tabulate ?(start = 0) (stop : int) (s : 'a stream) : 'a list = 
    let rec make_list i = if i = stop + 1 then [] else s i :: make_list (i + 1) in make_list start 
end


let s1 (n : int) = n;;
let s2 (n : int) = String.make n 'a'

let s3 (n : int) = List.init (n + 1) (fun i -> i)