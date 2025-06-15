open Syntax
let parse_file fname =
  let lexbuf = Lexing.from_channel (open_in fname) in
  Lexing.set_filename lexbuf fname;
  try 
    Parser.main Lexer.token lexbuf
  with
  | Parser.Error -> failwith "dupa"


let parse (s : string)  =
  Parser.main Lexer.token (Lexing.from_string s)
  
let equal f1 f2 = Formula.equal f1 f2


type expr_data =
  | EVar     of prf_var
  | ELet     of prf_var * expr_data * expr_data
  | EFun     of prf_var * formula * expr_data
  | EApp     of expr_data * expr_data
  | ETermFun of term_var * expr_data
  | ETermApp of expr_data * term
  | ERelApp  of expr_data * term_var * formula
  | EPair    of expr_data * expr_data
  | EFst     of expr_data
  | ESnd     of expr_data
  | ELeft    of expr_data * formula
  | ERight   of expr_data * formula
  | ECase    of expr_data * prf_var * expr_data * prf_var * expr_data
  | EAbsurd  of expr_data * formula
  | EPack    of term * expr_data * formula
  | EUnpack  of term_var * prf_var * expr_data * expr_data


type no_pos = 
    | Axiom of string * formula
    | Theorem of string * formula * expr_data

let rec better_expr (p : expr) : expr_data = 
  match p.data with
  | EVar     n                      -> EVar     n                     
  | ELet     (n, e1,e2)             -> ELet     (n, better_expr e1,better_expr e2)             
  | EFun     (n,f,e)                -> EFun     (n,f,better_expr e)               
  | EApp     (e1, e2)               -> EApp     (better_expr e1, better_expr e2)              
  | ETermFun (t,e)                  -> ETermFun (t,better_expr e)                 
  | ETermApp (e1, t)                -> ETermApp (better_expr e1, t)               
  | ERelApp  (e1, tv,f)             -> ERelApp  (better_expr e1, tv,f)            
  | EPair    (e1, e2)               -> EPair    (better_expr e1, better_expr e2)              
  | EFst     (e1 )                  -> EFst     (better_expr e1 )                 
  | ESnd     (e1 )                  -> ESnd     (better_expr e1 )                 
  | ELeft    (e1, f)                -> ELeft    (better_expr e1, f)                
  | ERight   (e1, f)                -> ERight   (better_expr e1, f)                
  | ECase    (e1, n1, e2, n2, e3)   -> ECase    (better_expr e1, n1, better_expr e2, n2, better_expr e3)   
  | EAbsurd  (e1, f)                -> EAbsurd  (better_expr e1, f)                
  | EPack    (t, e1, f)             -> EPack    (t, better_expr e1, f)             
  | EUnpack  (t, n, e1, e2)         -> EUnpack  (t, n, better_expr e1, better_expr e2) 

let rec better_data (d : def list) : no_pos list= 
  match d with
  | [] -> []
  | Axiom(_,name,f) :: xs' -> Axiom(name,f) :: better_data xs'
  | Theorem(_,name,f,proof) :: xs' -> Theorem(name,f,better_expr proof) :: better_data xs'