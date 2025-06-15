open Ast

let parse (s : string) : expr =
  Parser.main Lexer.read (Lexing.from_string s)

type value =
  | VInt of int
  | VBool of bool
  | VUnit
  | VPair of value * value

let eval_op (op : bop) (val1 : value) (val2 : value) : value =
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

let eval_type (c_type : type_check) (e : value) : value = 
  match c_type, e with 
  | Check_number , VInt _ -> VBool true 
  | Check_bool , VBool _ -> VBool true
  | Check_unit , VUnit -> VBool true
  | Check_pair , VPair _ -> VBool true
  | _ -> VBool false

let rec subst (x : ident) (s : expr) (e : expr) : expr =
  match e with
  | Binop (op, e1, e2) -> Binop (op, subst x s e1, subst x s e2)
  | If (b, t, e) -> If (subst x s b, subst x s t, subst x s e)
  | Var y -> if x = y then s else e
  | Let (y, e1, e2) ->
      Let (y, subst x s e1, if x = y then e2 else subst x s e2)
  | Pair (e1, e2) -> Pair (subst x s e1, subst x s e2)
  | Fst e -> Fst (subst x s e)
  | Snd e -> Snd (subst x s e)
  | Match (e1, z, y, e2) -> Match (subst x s e1, z, y, (if x = z || x = y then e2 else subst x s e2))
  | Type (type_check, e1) -> Type (type_check , subst x s e1)
  | _ -> e

let rec reify (v : value) : expr =
  match v with
  | VInt a -> Int a
  | VBool b -> Bool b
  | VPair (v1, v2) -> Pair (reify v1, reify v2)
  | VUnit -> Unit


let rec closed ( e : expr ) : bool =
  match e with
  | Int _ -> true
  | Bool _ -> true
  | Unit -> true
  | Binop (_, e1, e2) ->
      closed e1 && closed e2
  | If (b, t, e) -> closed b && closed t && closed e
  | Let (x, e1, e2) ->
    closed e1 && closed (subst x e1 e2) 
  | Var _ -> false
  | Type (_ , e1) -> closed e1
  | Pair (e1, e2) -> closed e1 && closed e2
  | Match (e1, x, y, e2) -> closed e1 && closed (subst x Unit (subst y Unit e2))
  | Fst (e1) -> let e2 = match e1 with Pair (x,_) -> x| _ -> failwith "fst of not a pair" in closed e2
  | Snd (e1) -> let e2 = match e1 with Pair (_,x) -> x| _ -> failwith "fst of not a pair" in closed e2

let rec eval (e : expr) : value =
  match e with
  | Int i -> VInt i
  | Bool b -> VBool b
  | Binop (And, e1, e2) -> let f = match eval e1 with | VBool b -> b | _ -> failwith "not bool" in 
    if not f then VBool false else eval_op And (eval e1) (eval e2)
  | Binop (Or, e1, e2) -> let f = match eval e1 with | VBool b -> b | _ -> failwith "not bool" in 
    if f then VBool true else eval_op Or (eval e1) (eval e2)
  | Binop (op, e1, e2) ->
      eval_op op (eval e1) (eval e2)
  | If (b, t, e) ->
      (match eval b with
        | VBool true -> eval t
        | VBool false -> eval e
        | _ -> failwith "type error")
  | Let (x , e1 , e2 ) -> if closed e1 then eval ( subst x e1 e2 ) else failwith "zmienna wolna w e1"
  | Pair (e1, e2) -> VPair (eval e1, eval e2)
  | Unit -> VUnit
  | Fst e ->
      (match eval e with
        | VPair (v1, _) -> v1
        | _ -> failwith "Type error")
  | Snd e ->
      (match eval e with
        | VPair (_, v2) -> v2
        | _ -> failwith "Type error")
  | Match (e1, x, y, e2) ->
      (match eval e1 with
        | VPair (v1, v2) ->
            e2
            |> subst x (reify v1)
            |> subst y (reify v2)
            |> eval
        | _ -> failwith "Type error")
  | Type (c_type , e1) -> eval_type c_type (eval e1)
  | Var x -> failwith ("unknown var " ^ x)

let interp (s : string) : value =
  eval (parse s)


(* let y = x in let x = 5 in y *)