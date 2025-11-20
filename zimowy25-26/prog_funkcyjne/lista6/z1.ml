type ('a, 'b) format = (string -> 'b) -> (string -> 'a)
(*zamienia kontynuacje w 'b na kontynuacje w 'a*)

let lit (s: string) : ('a, 'a) format = fun cont -> fun s' -> cont (s ^ s')

let int : (int    -> 'a, 'a) format = fun (cont: string -> 'a) -> (fun s i  -> cont (s ^ string_of_int i))
let str : (string -> 'a, 'a) format = fun (cont: string -> 'a) -> (fun s s' -> cont (s ^ s'))

(*

to jest zly zamysl tej funckji

let (^^) (type a b c ) (dr1 : (a -> b,b) format) (dr2 : (c -> b,b) format) : (a -> c -> b,b) format =
(*'a ,'b format = (string -> 'b) -> (string -> 'a )*)
(*'c ,'b format = (string -> 'b) -> (string -> 'c)*)
(*'a -> 'c,'b format = (string -> 'b) -> (string -> 'a -> 'c)*)
(* typuje się, ale nie uzylem dr1*)
(*prawie dobrze tylko nie musi to byc koniecznie funckcja moze to być literal*)
    fun (cont : string -> b) -> (fun s (args1) (args2) -> dr1 (fun s' -> dr2 cont s' args2) s args1)
 
*)
let (^^) (dr1 : ('a,'b) format) (dr2 : ('b,'c) format) : ('a,'c) format = 
    fun (cont : string -> 'c) s -> dr1 (fun (s' : string) -> dr2 cont s') s

let join_ints (dr1 : (int -> 'a,'a) format) (dr2 : (int -> int-> 'a,'a) format) : (int -> int -> int -> 'a, 'b) format = 
    fun (cont : string -> 'a) -> (fun (s : string) (i1 : int) (i2 : int) (i3 : int) -> dr2 (fun s' -> dr1 cont s' i1) s i2 i3)
(* doczepanie powinno byc takie ze dr1 oczekuje 'a i zwroci ostatecznie 'b, a dr2 oczekuje 'c i zwroci 'd
laczac to w jedno bedziemy oczekiwac 'a -> 'c i zwrocimy 'b wiec 'b = 'd, chcemy zwrocic ten sam typ ostatecznie,
chociaz nie do konca tak bo 'a  = int -> string -> 'b mowi ze oczekujemy int i string i zwracamy 'b
a 'c = string -> string -> 'b mowi ze oczekujemy string i string i zwracamy 'b, wiec ostatecznie chcemy funkcje
ktora bedzie oczekiwala int string string string i zwracala 'b, wiec jej typ nie bedzie 'a -> 'c, ale jak wydobyć z 'a wszystkie
zagniezdzone wartosci*)
let sprintf (fmt : ('a,string) format) : 'a =  fmt (fun s -> s) ""
(*(string -> string) -> (string -> 'a) i chcemy wydobyć 'a*)
(* 'a jest typem "łańcucha" czego bedziemy potrzebować do wyprodukowania stringa*)
(*jest mamy cos co oczekuje int i str to chcemy miec cos co oczekuje int -> string*)
let a = sprintf (lit "Ala ma " ^^ int ^^ lit " kot" ^^ str ^^ lit ".")