open Ast

let parse (s : string) : expr =
  Parser.main Lexer.read (Lexing.from_string s)

module M = Map.Make(String)

module Loc = Int
module H = Map.Make(Loc)


type env = value M.t

and value =
  | VInt of int
  | VBool of bool
  | VUnit
  | VPair of value * value
  | VClosure of ident * expr * env
  | VRecClosure of ident * ident * expr * env
  | VRef of Loc.t
  | Break
  | Continue
  
type heap = value H.t

let fresh h = H.cardinal h

let rec show_value v =
  match v with
  | VInt n -> string_of_int n
  | VBool v -> string_of_bool v
  | VUnit -> "()"
  | VPair (v1,v2) -> "(" ^ show_value v1 ^ ", " ^ show_value v2 ^ ")"
  | VClosure _ | VRecClosure _ -> "<fun>"
  | VRef _ -> "<ref>"
  | _ -> failwith "not impemented"

module type COMP = sig
  type 'a comp
  val return  : 'a -> 'a comp
  val fail : 'a comp
  val bind    : 'a comp -> ('a -> 'b comp) -> 'b comp

  val refc : value -> Loc.t comp
  val derefc : Loc.t -> value comp
  val assgn : Loc.t -> value -> unit comp
  val or_else : 'a comp -> 'a comp -> 'a comp
  val final_return : 'a comp -> 'a 
end

module TransactionComp : COMP = struct
  type 'a comp = heap -> ('a * heap) option
  let return (v : 'a) : 'a comp = fun h -> Some(v,h)
  let fail = fun _ -> None
  let bind (c : 'a comp) (f : 'a -> 'b comp) : 'b comp = fun h -> (match c h with
  | Some(v,h) -> f v h
  | None -> None)
  let refc (v : value) : Loc.t comp =
    fun h ->
      let r = fresh h in
      Some (r, H.add r v h)
  
  let derefc (l : Loc.t) : value comp =
    fun h -> Some (H.find l h, h)
  
  let assgn (l : Loc.t) (v : value) : unit comp =
    fun h -> Some ((), H.add l v h)

  let or_else (e1 : 'a comp) (e2 : 'a comp) : 'a comp = 
    fun h -> (match e1 h with
    | Some(v,h) -> Some(v,h)
    | None -> e2 h)

  let final_return (c : 'a comp) : 'a =
    match c H.empty with
    | Some (v,_) -> v
    | None -> failwith "blad w final_return"
end

(* Global comp, bo wywalając błąd nie zmienaimy stanu *)
open TransactionComp
let (let* ) = bind

(* value -> value ref *)


let int_op op v1 v2 =
  match v1, v2 with
  | VInt x, VInt y -> return (VInt (op x y))
  | _ -> failwith "type error"

let cmp_op op v1 v2=
  match v1, v2 with
  | VInt x, VInt y -> return (VBool (op x y))
  | _ -> failwith "type error"

let bool_op op v1 v2=
  match v1, v2 with
  | VBool x, VBool y -> return (VBool (op x y))
  | _ -> failwith "type error"

let eval_op (op : bop) : value -> value -> value comp =
  match op with
  | Add  -> int_op ( + )
  | Sub  -> int_op ( - )
  | Mult -> int_op ( * )
  | Div  -> int_op ( / )
  | And  -> bool_op ( && )
  | Or   -> bool_op ( || )
  | Eq   -> (fun v1 v2-> return (VBool (v1 = v2)))
  | Neq  -> (fun v1 v2-> return (VBool (v1 <> v2)))
  | Leq  -> cmp_op ( <= )
  | Lt   -> cmp_op ( < )
  | Geq  -> cmp_op ( >= )
  | Gt   -> cmp_op ( > )
  | Assgn -> (fun v1 v2 ->
      match v1 with
      | VRef r ->
        let* _ = assgn r v2 in
        return VUnit
      | _ -> failwith "type error")

let rec eval_env (env : env) (e : expr) : value comp =
  match e with
  | Int i -> return (VInt i)
  | Bool b -> return (VBool b)
  | Binop (op, e1, e2) ->
      bind (eval_env env e1) (fun v1 -> (* let* v1 = eval_env env e1 in *)
      bind (eval_env env e2) (fun v2 ->
      eval_op op v1 v2))
  | If (b, t, e) ->
      let* v = eval_env env b in
      (match v with
      | VBool true -> eval_env env t
      | VBool false -> eval_env env e
      | _ -> failwith "type error")
  | Var x ->
      let v =
        match M.find_opt x env with
        | Some v -> v
        | None -> failwith "unknown var"
      in
      return v
  | Let (x, e1, e2) ->
      let* v1 = eval_env env e1 in
      eval_env (env |> M.add x v1) e2
  | Pair (e1, e2) ->
      let* v1 = eval_env env e1 in
      let* v2 = eval_env env e2 in
      return (VPair (v1, v2))
  | Unit -> return VUnit
  | Fst e ->
      let* v = eval_env env e in
      (match v with
        | VPair (v1, _) -> return v1
        | _ -> failwith "Type error")
  | Snd e ->
      let* v = eval_env env e in
      (match v with
      | VPair (_, v2) -> return v2
      | _ -> failwith "Type error")
  | Match (e1, x, y, e2) ->
      let* v1 = eval_env env e1 in
      (match v1 with
      | VPair (v1, v2) ->
        eval_env (env |> M.add x v1 |> M.add y v2) e2
      | _ -> failwith "Type error")
  | IsPair e ->
      let* v = eval_env env e in
      let v =
        match v with
        | VPair _ -> VBool true
        | _ -> VBool false
      in
      return v
  | Fun (x, e) -> return (VClosure (x, e, env))
  | Funrec (f, x, e) -> return (VRecClosure (f, x, e, env))
  | App (e1, e2) ->
      let* v1 = eval_env env e1 in
      let* v2 = eval_env env e2 in
      (match v1 with
        | VClosure (x, body, clo_env) ->
            eval_env (M.add x v2 clo_env) body
        | VRecClosure (f, x, body, clo_env) as c ->
            eval_env (clo_env |> M.add x v2 |> M.add f c) body
        | _ -> failwith "not a function")
  | Ref e ->
      let* v = eval_env env e in
      let* r = refc v in
      return (VRef r)
  | Deref e ->
      let* v = eval_env env e in
      (match v with
        | VRef r -> derefc r
        | _ -> failwith "not a reference")
  | While(e1 ,e2) -> 
    let* v1 = eval_env env e1 in
    (match v1 with
    | VBool true -> let* v2 = eval_env env e2 in (match v2 with 
      | Break -> return VUnit
      | Continue -> eval_env env (While(e1 ,e2))
      | _ -> eval_env env (While(e1 ,e2)))
    | VBool false -> return VUnit
    | _ -> failwith "wrong type in while cond")
  | Break -> return Break
  | Continue -> return Continue
  | Try(e1, e2) -> or_else (eval_env env e1) (eval_env env e2)
  | Throw -> fail


let eval e = final_return (eval_env M.empty e)

let interp (s : string) : value =
  eval (parse s)
