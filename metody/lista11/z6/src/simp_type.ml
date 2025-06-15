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

module StrTypOrd = struct
  type t = string * typ

  let compare (s1, t1) (s2, t2) =
    let c = String.compare s1 s2 in
    if c <> 0 then c else 
     match t1, t2 with
      | TInt, TInt | TBool, TBool | TUnit, TUnit | TPair _ , TPair _ | TArrow _, TArrow _ -> 0
      | _ -> 1
end
module MEnv = Map.Make(StrTypOrd)
type menv = (string*typ) MEnv.t


let parse (s : string) : expr =
  Parser.main Lexer.read (Lexing.from_string s)

  
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

let simpl_infer (e : expr) : Ast.expr * typ =
  let curr_var = ref (-1) in 
  let new_var () = 
    curr_var := 1 + !curr_var;
    !curr_var 
  in 
  let rec simpl_infer env (method_env : menv) (e : expr) : Ast.expr * typ =
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
    let e1,t1 = simpl_infer env method_env e1 in 
    let e2,t2 = simpl_infer env method_env e2 in 
    check_type t1 TInt (binop_name op);
    check_type t2 TInt (binop_name op);
    builtin_binop op e1 e2,TInt
  | Binop((And | Or as op), e1, e2) ->
    let e1,t1 = simpl_infer env method_env e1 in 
    let e2,t2 = simpl_infer env method_env e2 in 
    check_type t1 TBool (binop_name op);
    check_type t2 TBool (binop_name op);
    builtin_binop op e1 e2,TInt
  | Binop((Leq | Lt | Geq | Gt as op), e1, e2) ->
    let e1,t1 = simpl_infer env method_env e1 in 
    let e2,t2 = simpl_infer env method_env e2 in 
    check_type t1 TInt (binop_name op);
    check_type t2 TInt (binop_name op);
    builtin_binop op e1 e2,TBool
  | Binop((Eq | Neq as op), e1, e2) ->
    let e1,t1 = simpl_infer env method_env e1 in
    let e2,t2 = simpl_infer env method_env e2 in
    check_type t1 t2 (binop_name op);
    builtin_binop op e1 e2,TBool
  | If(b, e1, e2) ->
    let b,t = simpl_infer env method_env b in 
    check_type t TBool "Not a bool in If cond";
    let e1,t1 = simpl_infer env method_env e1 in
    let e2,t2 = simpl_infer env method_env e2 in 
    check_type t1 t2 "e2 wrong type in If else";
    If( b, e1, e2),t2
  | Let(x, e1, e2) ->
    let e1,t1 = simpl_infer env method_env e1 in
    let e2,t2 = simpl_infer (Env.add_var env x t1) method_env e2 in
    Let(x , e1, e2) , t2
  | Pair(e1, e2) ->
    let e1,t1 = simpl_infer env method_env e1 in 
    let e2,t2 = simpl_infer env method_env e2 in
    Pair(e1, e2) , TPair(t1, t2)
  | App(e1, e2) ->
    begin match simpl_infer env method_env e1 with
    | e1,TArrow(tp2, tp1) ->
      let e2,t2 = simpl_infer env method_env e2 in 
      check_type t2 tp2 "e2 wrong type in App";
      App(e1,e2),tp1
    | _ -> failwith "e1 not a function in App"
    end
  | Fst e ->
    begin match simpl_infer env method_env e with
    | e,TPair(tp1, _) -> App(Builtin "", e),tp1
    | _ -> failwith "Not a Pair in "
    end
  | Snd e ->
    begin match simpl_infer env method_env e with
    | e,TPair(_, tp2) -> App(Builtin "snd", e),tp2
    | _ -> failwith "Not a Pair in Snd"
    end
  | Fun(x, tp1, e) ->
    let e,t = simpl_infer (Env.add_var env x tp1) method_env e in
    Fun(x,e),TArrow(tp1, t)
  | Funrec(f, x, tp1, tp2, e) ->
    let env = Env.add_var env x tp1 in
    let env = Env.add_var env f (TArrow(tp1, tp2)) in
    let e,t = simpl_infer env method_env e in
    check_type t tp2 "Wrong return type";
    Funrec(f,x,e),TArrow(tp1, tp2)
  | DotOp(e,n) -> 
    let e,t = simpl_infer env method_env e in 
    (match MEnv.find_opt (n,t) method_env with
    | Some (meth,t1) -> App(Var meth,e),t1
    | None -> failwith "unknown method of given type")
  | Method(m,x,t,e1,e2) -> 
    let new_m = m ^ string_of_int (new_var ()) in
    let e1,t1 = simpl_infer (Env.add_var env x t) method_env e1 in 
    let e2,t2 = simpl_infer env (MEnv.add (m,t) (new_m,t1) method_env) e2 in
    Let(new_m, Fun(x,e1),e2),t2

  and check_type tp1 tp2 ex =
  if tp1 = tp2 then ()
  else
    failwith ex 

  in simpl_infer Env.initial MEnv.empty e


let check_program e =
  let e,_ = simpl_infer e in
  e

(* simpl_infer (parse "method (x : int).toInt = x in method (x : bool).toInt = if x = false then 0 else 1 in (1.toInt) + (true.toInt)");; *)