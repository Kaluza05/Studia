open RawAst

exception Type_error of
  (Lexing.position * Lexing.position) * string

module Env = struct
  module StrMap = Map.Make(String)
  type t = typ StrMap.t

  let initial = StrMap.empty

  let add_var env x tp =
    StrMap.add x tp env

  let lookup_var env x =
    StrMap.find_opt x env
end

let binop_name (bop : RawAst.bop) : string =
  match bop with
  | Add  -> "add"
  | Sub  -> "sub"
  | Mult -> "mult"
  | Div  -> "div"
  | And  -> "and"
  | Or   -> "or"
  | Eq   -> "eq"
  | Neq  -> "neq"
  | Leq  -> "leq"
  | Lt   -> "lt"
  | Geq  -> "geq"
  | Gt   -> "gt"


let builtin_binop op e1 e2  : Ast.expr= App(App(Builtin(binop_name op), e1), e2)

let rec infer_type env (e : expr) : Ast.expr * typ =
  match e.data with
  | Unit   -> Unit,TUnit
  | Int  n -> Int n , TInt
  | Bool b -> Bool b, TBool
  | Var  x ->
    begin match Env.lookup_var env x with
    | Some tp -> Var x,tp
    | None    ->
      raise (Type_error(e.pos,
        Printf.sprintf "Unbound variable %s" x))
    end
  | Binop((Add | Sub | Mult | Div as op), e1, e2) ->
    let e1,t1 = infer_type env e1 in 
    let e2,t2 = infer_type env e2 in 
    check_type t1 TInt (binop_name op);
    check_type t2 TInt (binop_name op);
    builtin_binop op e1 e2,TInt
  | Binop((And | Or as op), e1, e2) ->
    let e1,t1 = infer_type env e1 in 
    let e2,t2 = infer_type env e2 in 
    check_type t1 TBool (binop_name op);
    check_type t2 TBool (binop_name op);
    builtin_binop op e1 e2,TInt
  | Binop((Leq | Lt | Geq | Gt as op), e1, e2) ->
    let e1,t1 = infer_type env e1 in 
    let e2,t2 = infer_type env e2 in 
    check_type t1 TInt (binop_name op);
    check_type t2 TInt (binop_name op);
    builtin_binop op e1 e2,TBool
  | Binop((Eq | Neq as op), e1, e2) ->
    let e1,t1 = infer_type env e1 in
    let e2,t2 = infer_type env e2 in
    check_type t1 t2 (binop_name op);
    builtin_binop op e1 e2 ,TBool
  | If(b, e1, e2) ->
    let b,t = infer_type env b in 
    check_type t TBool "Not a bool in If cond";
    let e1,t1 = infer_type env e1 in
    let e2,t2 = infer_type env e2 in 
    check_type t1 t2 "e2 wrong type in If else";
    If(b,e1,e2),t2
  | Let(x, e1, e2) ->
    let e1,t1 = infer_type env e1 in
    let e2,t2 = infer_type (Env.add_var env x t1) e2 in
    Let(x , e1, e2) , t2
  | Pair(e1, e2) ->
    let e1,t1 = infer_type env e1 in 
    let e2,t2 = infer_type env e2 in
    Pair(e1, e2) , TPair(t1, t2)
  | App(e1, e2) ->
    begin match infer_type env e1 with
    | e1,TArrow(tp2, tp1) ->
      let e2,t2 = infer_type env e2 in 
      check_type t2 tp2 "e2 wrong type in App";
      App(e1,e2),tp1
    | _ -> failwith "e1 not a function in App"
    end
  | Fst e ->
    begin match infer_type env e with
    | e,TPair(tp1, _) -> App(Builtin "fst", e),tp1
    | _ -> failwith "Not a Pair in Fst"
    end
  | Snd e ->
    begin match infer_type env e with
    | e,TPair(_, tp2) -> App(Builtin "snd", e),tp2
    | _ -> failwith "Not a Pair in Snd"
    end
  | Fun(x, tp1, e) ->
    let e,t = infer_type (Env.add_var env x tp1) e in
    Fun(x,e),TArrow(tp1, t)
  | Funrec(f, x, tp1, tp2, e) ->
    let env = Env.add_var env x tp1 in
    let env = Env.add_var env f (TArrow(tp1, tp2)) in
    let e,t = infer_type env e in
    check_type t tp2 "Wrong return type";
    Funrec(f,x,e),TArrow(tp1, tp2)

and check_type tp1 tp2 ex =
  if tp1 = tp2 then ()
  else
    failwith ex 

let check_program e =
  let e,_ = infer_type Env.initial e in
  e
