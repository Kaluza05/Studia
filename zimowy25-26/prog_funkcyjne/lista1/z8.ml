type cbool = { cbool : 'a. 'a-> 'a-> 'a }
type cnum = { cnum : 'a. ('a -> 'a) -> 'a -> 'a }

let ctrue = {cbool = fun x _ -> x}
let cfalse = {cbool = fun _ y -> y}
let cand (b1 : cbool) (b2 : cbool) : cbool = 
  { cbool = fun x y -> b1.cbool (b2.cbool x y) y }
let cor (b1 : cbool) (b2 : cbool) : cbool = 
  { cbool = fun x y -> b1.cbool x (b2.cbool x y) }
let cbool_of_bool (b : bool) : cbool = if b then ctrue else cfalse
let bool_of_cbool (f : cbool) : bool = f.cbool true false


let zero = { cnum = fun _ x -> x}  
let succ (value :  cnum) :  cnum = 
  {cnum = fun f x -> f (value.cnum f x) }
let add (value1 : cnum) (value2 :  cnum) : (cnum) = 
  {cnum = fun f x -> value1.cnum f (value2.cnum f x)  }
let mul (value1 : cnum) (value2 :  cnum) : (cnum) =
  {cnum = fun f x -> value1.cnum (value2.cnum f) x}
let is_zero  (value : cnum) : cbool = 
  {cbool = fun x y -> value.cnum (fun _ -> y) x}
let rec cnum_of_int  (i : int) : (cnum) = 
  if i = 0 then zero else succ (cnum_of_int (i - 1))
let int_of_cnum  (value : cnum) : int = 
  value.cnum (fun n -> n + 1) 0
