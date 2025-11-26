module type Monad = sig
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

type symbol = string
type 'v term =
| Var of 'v
| Sym of symbol * 'v term list

module TermMonad : Monad with type 'a t = 'a term = struct
  type 'a t = 'a term

  let return x = Var(x)
  let rec bind t f = 
    match t with
    | Var(v) -> f v
    | Sym(s,ts) -> Sym(s,List.map (fun v -> bind v f) ts)
end

(*bind odpoiwada podstawieniom za termy*)

let t1 = Sym("f", [Var "x"; Sym("g", [Var "y"])])
let f v =
  match v with
  | "x" -> Sym("h", [Var "y"])
  | x   -> Var x

let t2 = TermMonad.bind t1 f