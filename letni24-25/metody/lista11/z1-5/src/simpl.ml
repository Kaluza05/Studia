let parse (s : string) : RawAst.expr =
  Parser.main Lexer.read (Lexing.from_string s)

let binop_name (bop : RawAst.bop) =
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



module VEnv = Map.Make(String)
type venv = int VEnv.t

let simplify (e : RawAst.expr) : Astint.expr =
  let curr_var = ref (-1) in 
  let new_var () = 
    curr_var := 1 + !curr_var;
    !curr_var
  in
  let rec simplify (e : RawAst.expr) (var_env : venv) : Astint.expr =
  match e.data with
  | Unit   -> Unit
  | Int  n -> Int n
  | Bool b -> Bool b
  | Var  x -> (match VEnv.find_opt x var_env with
        | Some i -> Var(i)
        | None -> Var(new_var ()))
  | Binop(bop, e1, e2) ->
      App(App(Builtin(binop_name bop), simplify e1 var_env),
        simplify e2 var_env)
  | If(b, e1, e2)  ->
      If(simplify b var_env, simplify e1 var_env, simplify e2 var_env)
  | Let(x, e1, e2) ->
        let n = new_var () in 
      Let(n, simplify e1 var_env, simplify e2 (VEnv.add x n var_env))
  | Pair(e1, e2) ->
      Pair(simplify e1 var_env, simplify e2 var_env)
  | App(e1, e2) ->
      App(simplify e1 var_env, simplify e2 var_env)
  | Fst e ->
      App(Builtin "fst", simplify e var_env)
  | Snd e ->
      App(Builtin "snd", simplify e var_env)
  | Fun(x, _, e) ->
        let n = new_var () in 
      Fun(n, simplify e (VEnv.add x n var_env))
  | Funrec(f, x, _, _, e) ->
        let n = new_var () in 
        let m = new_var () in 
      Funrec(n, m, simplify e (VEnv.add f n (VEnv.add x m var_env)))
  in simplify e VEnv.empty