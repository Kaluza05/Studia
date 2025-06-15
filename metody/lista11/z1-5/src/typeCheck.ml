open RawAst
type pos = Lexing.position * Lexing.position
type except = pos * string
exception Type_error of except

module Env = struct
  module StrMap = Map.Make(String)
  type t = typ StrMap.t

  let initial = StrMap.empty

  let add_var env x tp =
    StrMap.add x tp env

  let lookup_var env x =
    StrMap.find_opt x env
end

let pp_pos ((start_p, end_p) : Lexing.position * Lexing.position) =
  if start_p.pos_lnum = end_p.pos_lnum then
    (* Ten sam wiersz: wypisz zakres kolumn *)
    Printf.sprintf "%s:%d:%d-%d"
      start_p.pos_fname
      start_p.pos_lnum
      (start_p.pos_cnum - start_p.pos_bol + 1)
      (end_p.pos_cnum - end_p.pos_bol)
  else
    (* Różne wiersze: pokaż tylko początek *)
    Printf.sprintf "%s:%d:%d"
      start_p.pos_fname
      start_p.pos_lnum
      (start_p.pos_cnum - start_p.pos_bol + 1)

let parse (s : string) : expr =
  Parser.main Lexer.read (Lexing.from_string s)

let rec infer_type (env : Env.t) (e : expr) (exn_list : except list) : typ * (except list) =
  match e.data with
  | Unit   -> TUnit, exn_list
  | Int  _ -> TInt,  exn_list
  | Bool _ -> TBool, exn_list
  | Var  x ->
    begin match Env.lookup_var env x with
    | Some tp -> tp,exn_list
    | None    ->
      TUnit,(e.pos, Printf.sprintf "Unbound variable %s" x) :: exn_list
    end
  | Binop((Add | Sub | Mult | Div as op), e1, e2) ->
    let exn_list = check_type env e1 TInt (Simpl.binop_name op) exn_list in 
    let exn_list = check_type env e2 TInt (Simpl.binop_name op) exn_list in 
    TInt, exn_list
  | Binop((And | Or as op), e1, e2) ->
    let exn_list = check_type env e1 TBool (Simpl.binop_name op) exn_list in 
    let exn_list = check_type env e2 TBool (Simpl.binop_name op) exn_list in 
    TBool, exn_list
  | Binop((Leq | Lt | Geq | Gt as op), e1, e2) ->
    let exn_list = check_type env e1 TInt (Simpl.binop_name op) exn_list in 
    let exn_list = check_type env e2 TInt (Simpl.binop_name op) exn_list in 
    TBool, exn_list
  | Binop((Eq | Neq as op), e1, e2) ->
    let tp,exn_list = infer_type env e1 exn_list in
    let exn_list = check_type env e2 tp (Simpl.binop_name op) exn_list in 
    TBool, exn_list 
  | If(b, e1, e2) ->
    let exn_list = check_type env b TBool "Not a bool in If cond" exn_list in
    let tp,exn_list = infer_type env e1 exn_list in
    let exn_list = check_type env e2 tp "e2 wrong type in If else" exn_list in
    tp, exn_list
  | Let(x, e1, e2) ->
    let tp1, exn_list = infer_type env e1 exn_list in
    let tp2, exn_list = infer_type (Env.add_var env x tp1) e2 exn_list in
    tp2, exn_list
  | Pair(e1, e2) ->
    let tp1, exn_list = infer_type env e1 exn_list in 
    let tp2, exn_list = infer_type env e2 exn_list in
    TPair(tp1,tp2 ), exn_list
  | App(e1, e2) ->
    begin match infer_type env e1 exn_list with
    | TArrow(tp2, tp1),exn_list ->
      let exn_list = check_type env e2 tp2 "e2 wrong type in App" exn_list in
      tp1, exn_list
    | _ ->  TUnit,(e1.pos,"e1 not a function in App")::exn_list
    end
  | Fst e ->
    begin match infer_type env e exn_list with
    | TPair(tp1, _),exn_list -> tp1,exn_list
    | _ -> TUnit,(e.pos,"Not a Pair in Fst")::exn_list
    end
  | Snd e ->
    begin match infer_type env e exn_list with
    | TPair(_, tp2),exn_list -> tp2,exn_list
    | _ -> TUnit,(e.pos,"Not a Pair in Snd")::exn_list
    end
  | Fun(x, tp1, e) ->
    let tp2,exn_list = infer_type (Env.add_var env x tp1) e exn_list in
    TArrow(tp1, tp2),exn_list
  | Funrec(f, x, tp1, tp2, e) ->
    let env = Env.add_var env x tp1 in
    let env = Env.add_var env f (TArrow(tp1, tp2)) in
    let exn_list = check_type env e tp2 "Wrong return type" exn_list in
    TArrow(tp1, tp2),exn_list

and check_type (env : Env.t) (e : expr) (tp : typ) (ex : string)  (exn_list : except list) : except list =
  let tp',exn_list = infer_type env e exn_list  in
  if tp = tp' then exn_list
  else
    (e.pos,ex) :: exn_list

let check_program e =
  let _,exns = infer_type Env.initial e [] in
  if exns = [] then e
  else (
    List.iter (fun (p,ex)-> begin Printf.eprintf "Error at %s: %s\n" (pp_pos p) ex end) exns;
    failwith "errors")
