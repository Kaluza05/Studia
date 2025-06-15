type bop =
  | Add | Sub | Mult | Div
  | And | Or
  | Eq | Leq

type type_check = 
  | Check_number
  | Check_bool
  | Check_unit
  | Check_pair

type ident = string


type expr = 
  | Int      of int
  | Unit
  | Bool     of bool
  | Var      of ident
  | Binop    of bop * expr * expr
  | If       of expr * expr * expr
  | Pair     of expr * expr
  | Snd      of expr
  | Fst      of expr
  | Let      of ident * expr * expr
  | Match    of expr * ident * ident * expr
  | Type     of type_check * expr
