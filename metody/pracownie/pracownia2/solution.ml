type bop =
  (* arithmetic *)
  | Add | Sub | Mult | Div
  (* logic *)
  | And | Or
  (* comparison *)
  | Eq | Neq | Leq | Lt | Geq | Gt

type ident = string

type expr =
  | Int    of int
  | Binop  of bop * expr * expr
  | Bool   of bool
  | If     of expr * expr * expr
  | Let    of ident * expr * expr
  | Var    of ident
  | Cell   of int * int
  | Unit
  | Pair   of expr * expr
  | Fst    of expr
  | Snd    of expr
  | Match  of expr * ident * ident * expr
  | IsPair of expr
  | Fun    of ident * expr
  | Funrec of ident * ident * expr
  | App    of expr * expr

module M = Map.Make(String)
module C = Map.Make(Int)

type env = value M.t

and value =
  | VInt of int
  | VBool of bool
  | VUnit
  | VPair of value * value
  | VClosure of ident * expr * env
  | VRecClosure of ident * ident * expr * env

type cell_map = value C.t
type cell = int * int

exception CirclularDepentency

let eval_op (op : bop) (val1 : value) (val2 : value) : value =
  match op, val1, val2 with
  | Add,  VInt  v1, VInt  v2 -> VInt  (v1 + v2)
  | Sub,  VInt  v1, VInt  v2 -> VInt  (v1 - v2)
  | Mult, VInt  v1, VInt  v2 -> VInt  (v1 * v2)
  | Div,  VInt  v1, VInt  v2 -> VInt  (v1 / v2)
  | And,  VBool v1, VBool v2 -> VBool (v1 && v2)
  | Or,   VBool v1, VBool v2 -> VBool (v1 || v2)
  | Leq,  VInt  v1, VInt  v2 -> VBool (v1 <= v2)
  | Lt,   VInt  v1, VInt  v2 -> VBool (v1 < v2)
  | Gt,   VInt  v1, VInt  v2 -> VBool (v1 > v2)
  | Geq,  VInt  v1, VInt  v2 -> VBool (v1 >= v2)
  | Neq,  _,        _        -> VBool (val1 <> val2)
  | Eq,   _,        _        -> VBool (val1 = val2)
  | _,    _,        _        -> failwith "type error"

let rec eval_env (env : env) (e : expr) (hashmap : (int, value) Hashtbl.t) (row_size : int) : value =
  match e with
  | Int i  -> VInt i
  | Bool b -> VBool b
  | Binop (op, e1, e2) ->
      eval_op op (eval_env env e1 hashmap row_size) (eval_env env e2 hashmap row_size)
  | If (b, t, e) ->
      (match eval_env env b hashmap row_size with
        | VBool true  -> eval_env env t hashmap row_size
        | VBool false -> eval_env env e hashmap row_size
        | _ -> failwith "type error")
  | Var x ->
     (match M.find_opt x env with
       | Some v -> v
       | None -> failwith "unknown var")
  | Let (x, e1, e2) ->
      eval_env (M.add x (eval_env env e1 hashmap row_size) env) e2 hashmap row_size
  | Pair (e1, e2) -> VPair (eval_env env e1 hashmap row_size, eval_env env e2 hashmap row_size)
  | Unit -> VUnit
  | Fst e ->
      (match eval_env env e hashmap row_size with
        | VPair (v1, _) -> v1
        | _ -> failwith "Type error")

  | Snd e ->
      (match eval_env env e hashmap row_size with
        | VPair (_, v2) -> v2
        | _ -> failwith "Type error")
  | Match (_e1, _x, _y, _e2) ->
      failwith "Not implemented"
  | IsPair e ->
      (match eval_env env e hashmap row_size with
        | VPair _ -> VBool true
        | _ -> VBool false)
  | Fun (x, e) -> VClosure (x, e, env)
  | Funrec (f, x, e) -> VRecClosure (f, x, e, env)
  | App (e1, e2) ->
      let v1 = eval_env env e1 hashmap row_size in
      let v2 = eval_env env e2 hashmap row_size in
      (match v1 with
        | VClosure (x, body, clo_env) ->
            eval_env (M.add x v2 clo_env) body hashmap row_size
        | VRecClosure (f, x, body, clo_env) as c ->
            eval_env (clo_env |> M.add x v2 |> M.add f c) body hashmap row_size
        | _ -> failwith "not a function")
  | Cell (row, col) -> (match Hashtbl.find_opt hashmap (row*row_size+col) with
          | Some v -> v
          | None -> failwith (Printf.sprintf "not in cell map (%d, %d)"  row  col))


let get_cells (e : expr) : cell list =
  let rec it (e : expr) (used : cell list) : cell list =  
  match e with
  | Cell (row, col)      ->  (row,col) :: used
  | Int _                -> []
  | Bool _               -> []
  | Var _                -> []
  | Unit                 -> []
  | Binop (_, e1, e2)    -> (it e1 used ) @ (it e2 used )
  | If (e1, e2, e3)      -> (it e1 used ) @ (it e2 used ) @ (it e3 used )
  | Let (_, e1, e2)      -> (it e1 used ) @ (it e2 used )
  | Pair (e1, e2)        -> (it e1 used ) @ (it e2 used )
  | Match (e1, _, _, e2) -> (it e1 used ) @ (it e2 used )
  | Fst e                -> it e used
  | Snd e                -> it e used
  | IsPair e             -> it e used
  | Fun (_, e)           -> it e used
  | Funrec (_, _, e)     -> it e used
  | App (e1, e2)         -> (it e1 used ) @ (it e2 used )
  in it e []

let eval = eval_env M.empty

let n_elem_list (n : int) (start : int) : int list = List.init n (fun i -> start + i)

let cell_to_int (cols : int) (x,y : cell) : int = x * cols + y

let unflatten (hashmap : (int,value) Hashtbl.t) (cols : int) (rows : int)  : value list list =
  let rec it (curr_row : int) : value list list = 
    if curr_row = rows then []
    else
      List.map
        (fun x ->
          match Hashtbl.find_opt hashmap x with
          | Some v -> v
          | None -> failwith ("not found " ^ string_of_int x))
        (n_elem_list cols (cols * curr_row))
      :: it (curr_row + 1)
  in it 0 

(* search graph refers to searching through every cell (vertex) where edges are one-way conncetions between dependent cells*)

let eval_spreadsheet (s : expr list list) : value list list option = 
  let rows, cols, flat_s = List.length s, List.length (List.hd s), Array.of_list (List.flatten s)  in
  let evaluated_tb = Hashtbl.create (rows*cols) in 
  let rec dfs_eval (curr : int) (path : (int,unit) Hashtbl.t) : unit =
      if Hashtbl.mem evaluated_tb curr then () else
      if Hashtbl.mem path curr then raise CirclularDepentency 
      else
        let neighbours = List.map (cell_to_int cols) (get_cells (flat_s.(curr))) in 
        List.iter (fun h -> 
          Hashtbl.add path curr ();
          dfs_eval h path;
          Hashtbl.remove path curr;) 
          neighbours;
        let v = eval (flat_s.(curr)) evaluated_tb cols in
        Hashtbl.add evaluated_tb curr v 

  in
  let rec search_graph (not_visited : int list) : unit =
    match not_visited with
    | [] -> ()
    | curr :: not_visited -> 
      if Hashtbl.mem evaluated_tb curr then search_graph not_visited 
      else
        dfs_eval curr (Hashtbl.create 10);
        search_graph not_visited
  in 
  try 
    search_graph (n_elem_list (rows*cols) 0);
    Some(
      unflatten evaluated_tb cols rows)
  with 
    | CirclularDepentency -> None
    | _ -> failwith "other exceptions"