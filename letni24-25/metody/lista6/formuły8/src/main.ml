open Ast

let rec subst ( x : ident ) ( s : qbf ) ( f : qbf ) : qbf =
  match f with
  | Top -> Top
  | Bot -> Bot
  | Disj (v1 , v2) -> Disj (subst x s v1 , subst x s v2)
  | Conj (v1 , v2) -> Conj ( subst x s v1 , subst x s v2)
  | Not v -> Not ( subst x s v)
  | Forall (y,v) -> Forall (y, if x = y then v else subst x s v)
  | Exists (y,v) -> Exists (y, if x = y then v else subst x s v)
  | Var y -> if x = y then s else Var y 

let rec eval (v : qbf) : bool =
  match v with
  | Top -> true
  | Bot -> false
  | Disj (v1 , v2) -> (eval v1) || (eval v2)
  | Conj (v1 , v2) -> eval v1 && eval v2
  | Not v -> not (eval v)
  | Forall (x,v1) -> eval (subst x Top v1) && eval (subst x Bot v1)
  | Exists (x,v1) -> eval (subst x Top v1) || eval (subst x Bot v1)
  | Var _ -> failwith "zmienna wolna w formule"

let parse (s : string) : qbf =
  Parser.main Lexer.read (Lexing.from_string s)

  let interp (s : string) : bool =
    eval (parse s)