type 'a t =
  | Comp of (unit -> 'a)
  | Val  of 'a
  | Computing 

and 'a my_lazy = 'a t ref

let force (cell : 'a my_lazy) : 'a =
  match !cell with
  | Computing -> failwith "value is currently being computed"
  | Val (v)     -> v
  | Comp(f)     -> 
    cell := Computing;
    let v = f () in
    cell := Val(v);
    v

let fix (f : 'a my_lazy -> 'a) : 'a my_lazy = 
  let cell = ref (Computing) in
  cell := Comp(fun () -> f cell);
  cell


type 'a stream = Cons of 'a * 'a stream my_lazy

let rec ones = Cons(1,ref (Comp(fun () -> ones)))
let f1 = fix (force)

let stream_of_ones = fix (fun s -> Cons(1, s))
(*let stream_of_ones = fix (fun stream_of_ones -> Seq.Cons(1, stream_of_ones))*)

type 'a llist = Nil | Cons of 'a * 'a llist my_lazy

let rec ints_from n = fix (fun _ -> Cons(n,ints_from (n+1)))

let ints = force (ints_from 0)


(* getting prime numers like on the lecture with streams
filter
primes
is_prime*)


(* filter musi się liczyć leniwie
dopoki nie trafimy na element spelniajacy mozemy lecieć dalej, ale jak juz natrafimy na element ktory spelnia to 
odraczamy wykonanie dopoki nie bedzie nam potrzzebne*)

let rec filter (p : ('a -> bool)) (ls : 'a llist) : 'a llist = 
  match ls with
  | Nil -> Nil
  | Cons(x,ls') when p x -> Cons(x,ref (Comp(fun () -> (filter p (force ls')))))
  | Cons(_,ls') ->  filter p (force ls')

(* take while powinno się liczyć leniwie tez raczej, i dzialac jak fold right troche*)
let rec take_while (p : 'a -> bool) (ls : 'a llist) = 
    match ls with 
    | Cons(x,ls') when p x -> Cons(x, ref (Comp(fun () -> take_while p (force ls'))))
    | _ -> Nil

(* for_all gorliwie bo chcemy od razu poznać wartosc*)
let rec for_all (p : 'a -> bool) (ls : 'a llist) : bool = 
  match ls with
  | Nil -> true
  | Cons(x, ls') when p x -> for_all p (force ls')
  | _ -> false
let rec primes = Cons(2, ref (Comp(fun () -> filter is_prime (force (ints_from 3)))))
and is_prime n = 
  primes
  |> take_while (fun p -> p * p <= n)
  |> for_all (fun p -> n mod p <> 0)

let next (ls : 'a llist) : 'a llist = 
  match ls with
  | Nil -> failwith "aaaa"
  | Cons(x,ls') -> force ls'