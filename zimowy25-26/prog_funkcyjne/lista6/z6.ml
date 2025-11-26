open Proc

let rec echo k =
recv (fun v ->
send v (fun () ->
echo k))

(* type ('a,'z,'i,'o) proc = ('a -> ('z,'i,'o) ans) -> ('z,'i,'o) ans 
val send : 'o -> (unit,'z,'i,'o) proc
val recv : ('i,'z,'i,'o) proc
*)
let rec map (f : 'i -> 'o) : ('a, 'z, 'i, 'o) proc = 
  fun (cont : 'a -> ('z,'i,'o) ans) -> 
    recv (fun v -> 
    send (f v) (fun () -> map f cont) )

(* proc — proces, który nakłada podaną funkcję po kolei
na wszystkie elementy przeczytane z wejścia. Np. obliczenie *)
(* run (map String.length >|> map string_of_int) *)
(* powinno po kolei wyświetlać długości wprowadzanych wierszy. *)
let rec filter (p : 'i -> bool) :  ('a, 'z, 'i, 'i) proc = 
  fun (cont : 'a -> ('z,'i,'i) ans) -> 
    recv (fun v -> 
    if p v then send v (fun () -> filter p cont)
    else filter p cont)
(* proc — proces, który przekazuje dalej tylko te
wartości odczytane z wejścia, które spełniają podany predykat. Np. *)
(* run (filter (fun s -> String.length s >= 5)) *)
(* powinno wyświetlać tylko te wiersze ze standardowego wejścia, które mają co najmniej 5 znaków. *)
let rec nats_from (i : int) : ('a, 'z, 'i, int) proc = 
  fun (cont : 'a -> ('z,'i, int) ans ) -> 
    send i (fun () -> nats_from (i+1) cont)

(* proc — proces, który dla danego n wysyła na wyjście
wszystkie liczby naturalne zaczynając od n. *)
let rec sieve : ('a, 'a, int, int) proc = 
  (* jakies send n <|> proces filter (fun k -> k % n <> 0) *)
  fun cont -> 
    recv (fun n -> 
    send n (fun () -> (filter (fun k -> k mod n <> 0 ) >|> sieve) cont ))
(* proc — proces, który przesyła dalej pierwszą przeczytaną liczbę n,
a następnie zamienia się w swoją kopię złożoną z procesem, który przepuszcza tylko liczby niepodzielne
przez n (zastanów się w którą stronę lepiej jest złożyć te procesy). Takiego procesu powinno dać się
użyć do wyświetlenia wszystkich liczb pierwszych:
run (nats_from 2 >|> sieve >|> map string_of_int) *)