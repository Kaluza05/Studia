[@@@ocaml.warning "-8"]

module I = Interp

(* Składnia RPN *)
type ident = string
type cmd =
  | PushInt  of int
  | PushBool of bool
  | PushPair
  | PushUnit
  | PushVar of ident
  | Fst
  | Snd
  | IsPair
  | Binop    of Ast.bop
  | CndJmp   of prog * prog

and prog = cmd list

module M = Map.Make(String)
type env = I.value M.t
(* Kompilacja do RPN *)

let rec rpn_of_ast (expr : Ast.expr) : prog =
  Ast.(match expr with
  | Int n ->
      [PushInt n]
  | Bool b ->
      [PushBool b]
  | Var x -> [PushVar x]
  | Binop (op, e1, e2) ->
      rpn_of_ast e1 @ rpn_of_ast e2 @ [Binop op]
  | If (b, t, e) ->
      rpn_of_ast b @ [CndJmp (rpn_of_ast t, rpn_of_ast e)]
  | Pair (e1, e2) ->
      rpn_of_ast e1 @ rpn_of_ast e2 @ [PushPair]
  | Fst e ->
      rpn_of_ast e @ [Fst]
  | Snd e ->
      rpn_of_ast e @ [Snd]
  | Unit ->
      [PushUnit]
  | IsPair e ->
      rpn_of_ast e @ [IsPair]
  | _ ->
      failwith "not implemented")

(* Ewaluator dla RPN *)

(* ewaluator nie jest elementem procesu kompilacji, ale
 * przydaje się do testowania i debugowania
 *)
let rec eval_rpn (s : I.value list) (p : prog) (env : env) : I.value =
  match p, s with
    | [], [n] -> n
    | [], _   -> failwith "error!"
    | (PushInt n :: p'), _ ->
        eval_rpn (I.VInt n :: s) p' env
    | (PushBool b :: p'), _ ->
        eval_rpn (I.VBool b :: s) p' env
    | (PushVar x :: p'), _ ->
        (match M.find_opt x env with
        | Some v -> eval_rpn (v :: s) p' env
        | None -> failwith "x not in env")
    | (Binop op :: p'), (v2 :: v1 :: s') ->
        eval_rpn (I.eval_op op v1 v2 :: s') p' env
    | (CndJmp (t,_) :: p'), (I.VBool true :: s') ->
        eval_rpn s' (t @ p') env
    | (CndJmp (_,e) :: p'), (I.VBool false :: s') ->
        eval_rpn s' (e @ p') env
    | (PushPair :: p'), (v2 :: v1 :: s') ->
        eval_rpn (I.VPair (v1, v2) :: s') p' env
    | (Fst :: p'), (I.VPair (v1,_) :: s') ->
        eval_rpn (v1 :: s') p' env
    | (Snd :: p'), (I.VPair (_,v2) :: s') ->
        eval_rpn (v2 :: s') p' env
    | (PushUnit :: p'), (s') ->
        eval_rpn (I.VUnit :: s') p' env 
    | _ -> failwith "error!!"


let string_of_binop (s : Ast.bop) : string = 
  match s with
  | Add -> " + "
  | Sub -> " - "
  | Mult -> " * "
  | Div -> " / "
  | _ -> failwith "wrong string"
(* z.1 *)
let rec pn_of_ast (e : Ast.expr) : prog = 
  match e with
  | Int i -> [PushInt i]
  | Bool b -> [PushBool b]
  | Var x -> [PushVar x]
  | Unit -> [PushUnit]
  | Pair(e1,e2) -> PushPair :: (pn_of_ast e1 @ pn_of_ast e2)
  | Binop(op,e1,e2) -> (Binop op) :: (pn_of_ast e1 @ pn_of_ast e2)
  | Fst (e) -> Fst :: pn_of_ast e
  | Snd (e) -> Snd :: pn_of_ast e
  | IsPair (e) -> IsPair :: pn_of_ast e
  | If (b,t,e) -> CndJmp(pn_of_ast t , pn_of_ast e) :: (pn_of_ast b)

let rec string_pn_of_ast (e : Ast.expr) : string = 
  match e with
  | Int i -> string_of_int i ^ " "
  | Bool b -> string_of_bool b ^ " "
  | Var x -> x ^ " "
  | Unit -> "()"
  | Pair(e1,e2) -> "PushPair " ^ (string_pn_of_ast e1 ^ string_pn_of_ast e2)
  | Binop(op,e1,e2) -> (string_of_binop op) ^ (string_pn_of_ast e1 ^ string_pn_of_ast e2)
  | Fst (e) -> "Fst " ^ string_pn_of_ast e
  | Snd (e) -> "Snd " ^ string_pn_of_ast e
  | IsPair (e) -> "IsPair " ^ string_pn_of_ast e
  | If (b,t,e) ->  " CndJmp( " ^string_pn_of_ast t ^", "^ string_pn_of_ast e ^") " ^ (string_pn_of_ast b)

(* z.2,4 *)

let rec eval_pn (p : prog) (env : env) : I.value = 
    let rec it (p : prog) : I.value * prog =
        match p with
        | PushInt i :: p -> (VInt i, p) 
        | PushBool b :: p  -> (VBool b,p)
        | PushUnit :: p -> (VUnit,p)
        | PushVar x :: p -> (match M.find_opt x env with 
            | Some v -> (v,p)
            | None -> failwith "x not in env")
        | PushPair :: p -> 
            let (v1,p) = it p in
            let (v2,p) = it p in 
            (VPair(v1,v2),p)
        | Binop op :: p ->
            let (v1,p) = it p in
            let (v2,p) = it p in 
            (I.eval_op op v1 v2,p)
        | Fst :: p ->
            let (VPair(v1,_),p) = it p in
            (v1,p) 
        | Snd :: p ->
            let (VPair(_,v2),p) = it p in
            (v2,p) 
        | IsPair :: p -> 
            (match it p with
            | VPair _ , p -> (I.VBool true, p )
            | _ , p -> (I.VBool false, p ))
        | CndJmp(t,e) :: p -> 
            let (v1,p) = it p in 
            match v1 with
            | VBool true -> (eval_pn t env,p)
            | VBool false -> (eval_pn e env,p)
            | _ -> failwith " wrong if statement"
    in match it p with
        | v , [] -> v
        | _ -> failwith "sometinhg wrong"
        
(* z.3,5 *)
let rpn_stack_size (p : prog) : int =
  let rec it (p : prog) (mx : int) (curr : int): int = 
  match p with
  | [] -> mx
  | PushInt _  :: p' -> it p' (max (curr + 1) mx) (curr + 1) 
  | PushBool _ :: p' -> it p' (max (curr + 1) mx) (curr + 1)
  | PushVar _  :: p' -> it p' (max (curr + 1) mx) (curr + 1)
  | PushUnit   :: p' -> it p' (max (curr + 1) mx) (curr + 1)
  | Binop _    :: p' -> it p' mx (curr-1)
  | PushPair   :: p' -> it p' mx (curr-1)
  | Fst        :: p' -> it p' mx (curr)
  | Snd        :: p' -> it p' mx (curr)
  | IsPair     :: p' -> it p' mx (curr)
  | CndJmp (p1,p2) :: p' -> (max (it p1 0 0) (it p2 0 0)) + it p' (mx) (curr-1)
  in it p 0 0





(* Kompilacja RPN do podzbioru C *)

let lbl_cntr = ref 0 (* bleee!! *)

let fresh_lbl () =
  incr lbl_cntr;
  "lbl" ^ string_of_int !lbl_cntr

let emit_bop (op : Ast.bop) : string =
  Ast.(match op with
  | Add  -> "+"
  | Sub  -> "-"
  | Mult -> "*"
  | Div  -> "/"
  | And  -> "&&"
  | Or   -> "||"
  | Eq   -> "=="
  | Neq  -> "!="
  | Gt   -> ">"
  | Lt   -> "<"
  | Geq  -> ">="
  | Leq  -> "<=")

let emit_bop_res_tag (op : Ast.bop) : string =
  Ast.(match op with
  | Add | Sub | Mult | Div -> "INT"
  | _ -> "BOOL")

let emit_line (s : string) : string =
  "  " ^ s ^ ";\n"

let emit_lbl (s : string) : string =
  " " ^ s ^ ":\n"

(** allocate list of values, pop n elems from the stack*)
let alloc_pop (ss : string list) (to_pop : int) : string =
  (ss
   |> List.mapi (fun i s ->
        emit_line ("heap[heap_ptr+" ^ string_of_int i ^ "] = " ^ s))
   |> String.concat "") ^
  emit_line ("heap_ptr += " ^ string_of_int (List.length ss)) ^
  emit_line ("stack_ptr += " ^ string_of_int (1 - to_pop)) ^
  emit_line ("stack[stack_ptr] = heap_ptr - " ^ string_of_int (List.length ss - 1))

let show_cmd (c : cmd) : string =
  match c with
  | PushInt n -> emit_line ("// PushInt " ^ string_of_int n)
  | PushBool b -> emit_line ("// PushBool " ^ (if b then "true" else "false"))
  | Binop _op -> emit_line "// Binop"
  | PushPair -> emit_line "// PushPair"
  | CndJmp _ -> emit_line "// CndJmp"
  | Fst -> emit_line "// Fst"
  | Snd -> emit_line "// Snd"
  | PushUnit -> emit_line "// PushUnit"
  | IsPair -> emit_line "// IsPair"


let rec emit_cmd (c : cmd) : string =
  show_cmd c ^
  match c with
    | PushInt n ->
        alloc_pop ["INT"; string_of_int n] 0
    | PushBool n ->
        alloc_pop ["BOOL"; if n then "1" else "0"] 0
    | PushPair ->
        alloc_pop ["PAIR"; "stack[stack_ptr-1]"; "stack[stack_ptr]"] 2
    | PushUnit ->
        alloc_pop ["UNIT"] 0
    | Fst ->
        emit_line "stack[stack_ptr] = heap[stack[stack_ptr]]"
    | Snd ->
        emit_line "stack[stack_ptr] = heap[stack[stack_ptr]+1]"
    | IsPair ->
        alloc_pop ["BOOL"; "heap[stack[stack_ptr] - 1] == PAIR"] 1
    | Binop op -> ( match op with
    | Eq -> alloc_pop [emit_bop_res_tag Eq ;
          "structural_compare(stack[stack_ptr-1],stack[stack_ptr])"] 
              2
    | _ -> 
        alloc_pop
          [emit_bop_res_tag op;
           ("heap[stack[stack_ptr-1]] " ^ emit_bop op ^ " heap[stack[stack_ptr]]")]
          2)
    | CndJmp (t, e) ->
        let lbl_t = fresh_lbl () in
        let lbl_end = fresh_lbl () in
        emit_line ("if (heap[stack[stack_ptr]]) goto " ^ lbl_t) ^
        emit_line "stack_ptr--" ^
        emit e ^
        emit_line ("goto " ^ lbl_end) ^
        emit_lbl lbl_t ^
        emit_line "stack_ptr--" ^
        emit t ^
        emit_lbl lbl_end

and emit (p : prog) : string =
  List.fold_left (fun res cmd -> res ^ emit_cmd cmd) "" p

let compile (s : string) : string =
  s
  |> Interp.parse
  |> rpn_of_ast
  |> emit
  |> Runtime.with_runtime
