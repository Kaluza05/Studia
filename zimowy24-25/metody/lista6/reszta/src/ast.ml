type bop =
  | Add | Sub | Mult | Div
  | And | Or
  | Eq | Leq


type pairop = Fst | Snd

type ident = string

type expr = 
  | Int      of int
  | Unit     of unit
  | Bool     of bool
  | Var      of ident
  | Binop    of bop * expr * expr
  | If       of expr * expr * expr
  | Pair     of expr * expr
  | Pairop   of pairop * expr
  | Let      of ident * expr * expr
  | Sum      of ident * expr * expr * expr
  | Match    of expr * ident * ident * expr
  (* | Integral of ident * expr * expr * expr Integral from a to b f(x) dx *)
  (* | For      of ident * expr * expr * expr  For x  in m k f(X) *)