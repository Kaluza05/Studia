let ctrue (x : 'a) (y : 'a) : 'a = x
let cfalse (x : 'a) (y : 'a) : 'a = y
let cand (b1 : 'a -> 'a -> 'a) (b2 :  'a -> 'a -> 'a) :  ('a -> 'a -> 'a) = fun x y -> b1 (b2 x y) y
let cor (b1 : 'a -> 'a -> 'a) (b2 : 'a -> 'a -> 'a) : ('a -> 'a -> 'a)= fun x y -> b1 x (b2 x y)
let cbool_of_bool (b : bool) : ('a -> 'a -> 'a) = if b then ctrue else cfalse
let bool_of_cbool (f : bool -> bool -> bool) : bool = f true false

(* dlaczego to ma sens
cand cfalse _ od (x,y) zwraca y wiec cfalse
cand ctrue zwraca b2 (x y) czyli x jesli b2 = ctrue i y jesli b2 = cfalse wiec zgadza się

cor c_true _ zwraca x wiec ctrue
cor c_false  zwraca b2 x y wiec jesli b2 = ctrue to x wiec ctrue, b2 = cfalse zwraca y wiec cfalse

f jest ctrue zwraca true
f jest cfalse zwraca false

*)


let zero (_ : 'a -> 'a) (x : 'a) : 'a = x
let succ (value :  ('a-> 'a)-> 'a-> 'a) :  (('a-> 'a)-> 'a-> 'a) = 
  fun f x -> f (value f x)
let add (value1 : ('a-> 'a)-> 'a-> 'a) (value2 :  ('a-> 'a)-> 'a-> 'a) : (('a-> 'a)-> 'a-> 'a) = 
  fun f x -> value1 f (value2 f x)  
let mul (value1 : ('a-> 'a)-> 'a-> 'a) (value2 :  ('a-> 'a)-> 'a-> 'a) : (('a-> 'a)-> 'a-> 'a) =
  fun f x -> value1 (value2 f) x
let is_zero  (value : ('a-> 'a)-> 'a-> 'a) : ('a-> 'a-> 'a) = 
  fun x y -> value (fun _ -> y) x
let rec cnum_of_int  (i : int) : (('a-> 'a)-> 'a-> 'a) = 
  if i = 0 then zero else succ (cnum_of_int (i - 1))
let int_of_cnum  (value : (int-> int)-> int-> int) : int = value (fun n -> n + 1) 0

(* 
add returns f^n(f^m(x)) so f^(n+m)(x),
mul returns (f^n)^m(x) so f^(n*m)(x),
iszero returns cfalse if the function f was used (n > 0), and ctrue if it wasn't (n = 0)
f x -> f^n(x)
int_of_cnum (fun n -> n+1) 0 returns n , because 0 represents zero, fun n -< n+1 represents succesor, and f^n(0) is just n*)