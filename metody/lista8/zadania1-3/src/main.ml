open Ast

let parse (s : string) : expr =
  Parser.main Lexer.read (Lexing.from_string s)

type value =
  | VInt of int
  | VBool of bool

module M = Map.Make(String)
type env = expr M.t
type new_name = ident M.t

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

let rec subst (x : ident) (s : expr) (e : expr) : expr =
  match e with
  | Binop (op, e1, e2) -> Binop (op, subst x s e1, subst x s e2)
  | If (b, t, e) -> If (subst x s b, subst x s t, subst x s e)
  | Var y -> if x = y then s else e
  | Let (y, e1, e2) ->
      Let (y, subst x s e1, if x = y then e2 else subst x s e2)
  | _ -> e

let reify (v : value) : expr =
  match v with
  | VInt a -> Int a
  | VBool b -> Bool b

let rec eval (e : expr) : value =
  match e with
  | Int i -> VInt i
  | Bool b -> VBool b
  | Binop (op, e1, e2) ->
      eval_op op (eval e1) (eval e2)
  | If (b, t, e) ->
      (match eval b with
           | VBool true -> eval t
           | VBool false -> eval e
           | _ -> failwith "type error")
  | Let (x, e1, e2) ->
      eval (subst x (reify (eval e1)) e2)
  | Var x -> failwith ("unknown var " ^ x)

  let simplify (op:bop) (e1:expr) (e2:expr) = 
    match op,e1,e2 with
    | Add,  Int  v1, Int  v2 -> Int  (v1 + v2)
    | Sub,  Int  v1, Int  v2 -> Int  (v1 - v2)
    | Mult, Int  v1, Int  v2 -> Int  (v1 * v2)
    | Div,  Int  v1, Int  v2 -> Int  (v1 / v2)
    | And,  Bool v1, Bool v2 -> Bool (v1 && v2)
    | Or,   Bool v1, Bool v2 -> Bool (v1 || v2)
    | Leq,  Int  v1, Int  v2 -> Bool (v1 <= v2)
    | Eq,  _,         _      -> Bool (e1 = e2)
    | _,   _,        _       -> failwith "type error"


  let rec cp (e : expr) (env : env): expr = 
    match e  with
    | Int(e) -> Int(e)
    | Bool(e) -> Bool(e)
    | If(b,t,e) -> let b = cp b env in (match b with
      | Bool(true) -> cp t env
      | Bool(false) -> cp e env
      | _ -> If(b,cp t env, cp e env))
    | Var(x) -> (match M.find_opt x env with
      | Some(y) -> y
      | None -> Var(x))
    | Let(x,e1,e2) -> let v1 = cp e1 env in 
      (match v1 with
      | Int(_) -> cp e2 (M.add x v1 env)
      | Bool(_) -> cp e2 (M.add x v1 env)
      | _ -> Let(x,v1, cp e2 (M.add x (Var x) env )))
    | Binop(op, e1, e2) -> let (v1,v2) = (cp e1 env, cp e2 env) in 
      (match v1, v2 with
      | Int(_),Int(_) -> simplify op v1 v2
      | Bool(_),Bool(_) -> simplify op v1 v2
      | _ -> Binop(op , v1, v2))

let rename_expr (str:string) : expr = 
  let rec rename (e :expr) (curr_name :string) (env: new_name)= 
    match e with
    | Int(_) -> e
    | Bool(_) -> e
    | Binop(op,e1,e2) -> Binop(op,rename e1 (curr_name^"0") env, rename e2 (curr_name^"1") env)
    | If(b,t,e) -> If(rename b (curr_name^"0") env, rename t (curr_name^"1") env, rename e (curr_name^"2") env)
    | Var(x) -> (match M.find_opt x env with
      | Some (name) -> Var(name)
      | None -> Var(curr_name))
    | Let(x,e1,e2) -> Let(curr_name, rename e1 (curr_name^"0") env, rename e2 (curr_name^"1") (M.add x curr_name env))
  in rename (parse str) "#" M.empty 

let alpha_equiv (e1: expr) (e2: expr) : bool =
  let rec check_envs (e1:expr) (e2: expr) (env1: new_name) (env2: new_name) : bool = 
    match e1,e2 with
    | Int(i1),Int(i2) -> i1 = i2
    | Bool(b1), Bool(b2) -> b1 = b2
    | Var(x),Var(y) -> (match M.find_opt x env1, M.find_opt y env2 with
      | Some(x1), Some(y1) -> x1 = y && y1 = x
      | _,_ -> false)
    | Binop(op1,l1,r1),Binop(op2,l2,r2) -> if op1 <> op2 then false else (check_envs l1 l2 env1 env2) && (check_envs r1 r2 env1 env2)
    | If(b1,t1,e1), If(b2,t2,e2) -> (check_envs b1 b2 env1 env2) && (check_envs t1 t2 env1 env2) && (check_envs e1 e2 env1 env2)
    | Let(x,el1,er1), Let(y,el2,er2) -> (check_envs el1 el2 env1 env2) && (check_envs er1 er2 (M.add x y env1) (M.add y x env2))
    | _ -> false
  in check_envs e1 e2 M.empty M.empty

let run_cp (e : expr) = cp e M.empty
let interp (s : string) : value =
  eval (parse s)
