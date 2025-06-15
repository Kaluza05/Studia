open Ast

type value =
  | VInt of int
  | VBool of bool
  | VUnit of unit
  | VPair of value * value

let eval_bop (op : bop) (val1 : value) (val2 : value) : value =
  match op, val1, val2 with
  | Add,  VInt  v1, VInt  v2 -> VInt  (v1 + v2)
  | Sub,  VInt  v1, VInt  v2 -> VInt  (v1 - v2)
  | Mult, VInt  v1, VInt  v2 -> VInt  (v1 * v2)
  | Div,  VInt  v1, VInt  v2 -> VInt  (v1 / v2)
  | And,  VBool v1, VBool v2 -> VBool (v1 && v2)
  | Or,   VBool v1, VBool v2 -> VBool (v1 || v2)
  | Leq,  VInt  v1, VInt  v2 -> VBool (v1 <= v2)
  | Eq,   _,        _        -> VBool (val1 = val2)
  | _,    _,        _        -> failwith "type error"

let eval_pairop (op : pairop) (v : value) : value =
  match op, v with
  | Fst, VPair (f, _) -> f
  | Snd, VPair (_, s) -> s
  | _ -> failwith "not a pair"

let rec subst (x : ident) (s : expr) (e : expr) : expr =
  match e with
  | Binop (op, e1, e2)     -> Binop (op, subst x s e1, subst x s e2)
  | Pairop (op, e)         -> Pairop (op, subst x s e)
  | Pair (e1, e2)          -> Pair (subst x s e1, subst x s e2)
  | If (b, t, e)           -> If (subst x s b, subst x s t, subst x s e)
  | Let (y, e1, e2)        -> Let (y, subst x s e1, if x = y then e2 else subst x s e2)
  | Match (e1, i1, i2, e2) ->  Match ((subst x s e1), i1, i2, if x = i1 || x = i2 then e2 else (subst x s e2))
  | Sum (id, e1, e2, e3)   -> Sum (id , (subst x s e1) , (subst x s e2), if x = id then  e3 else (subst x s e3))
  | Var y -> if x = y then s else e
  | _ -> e

let rec reify (v : value) : expr =
  match v with
  | VInt  a -> Int a
  | VBool b -> Bool b
  | VUnit u -> Unit u
  | VPair (f, s) -> Pair (reify f, reify s)

let int_of_VInt (v : value) : int = match v with VInt i -> i | _ -> failwith "not a VInt"

let rec eval (e : expr) : value =
  match e with
  | Int i -> VInt i
  | Bool b -> VBool b
  | Unit u -> VUnit u
  | Pair (e1, e2) -> VPair (eval e1, eval e2)
  | Binop (op, e1, e2) -> eval_bop op (eval e1) (eval e2)
  | Pairop (op, e) -> eval_pairop op (eval e)
  | If (b, t, e) ->
      (match eval b with
           | VBool true -> eval t
           | VBool false -> eval e
           | _ -> failwith "type error")
  | Let (x, e1, e2) ->
      eval (subst x (reify (eval e1)) e2)
  | Var x -> failwith ("unknown var " ^ x)


  | Sum (x, e1, e2, e3) -> 
    let sum (x : ident) (st : int) (en : int) (ex : expr) = 
      let rec it y acc = 
        if y > en then acc else it (y+1) (int_of_VInt (eval (subst x (Int y) ex)) + acc) 
        in it st 0 in
    let st = int_of_VInt (eval e1) and en = int_of_VInt (eval e2)
    in VInt (sum x st en e3)



  | Match (e1, i1, i2, e2) -> let vpair = eval e1 in (
        match vpair with
        | VPair (f, s) -> eval (subst i1 (reify f) (subst i2 (reify s) e2))
        | _ -> failwith "matching not a pair")
  | _ -> failwith "evaluation not implemented"



let parse (s : string) : expr =
  Parser.main Lexer.read (Lexing.from_string s)

let interp (s : string) : value =
  eval (parse s)



let rec closed ( e : expr ) : bool = match e with
| Int _ -> true
| Bool _ -> true
| Unit _ -> true
| Binop (_, e1, e2) ->
    closed e1 && closed e2
| If (b, t, e) -> closed b && closed t && closed e
| Let (x, e1, e2) ->
    closed e1 && closed (subst x e1 e2)
| Var _ -> false
| _ -> failwith "not implemented"