type empty = |

module QBF = struct
type var = string
type formula =
| Var of var (* zmienne zdaniowe *)
| Bot (* spójnik fałszu (⊥)*)
| Not of formula (* negacja (¬φ)*)
| And of formula * formula (* koniunkcja (φ ∧ ψ)*)
| All of var * formula (* kwantyfikacja uniwersalna (∀p.φ)*)
type env = var -> bool
let rec eval (e : env) (f : formula) = 
  match f with
  | Var v -> e v
  | Bot -> false
  | Not f -> not (eval e f)
  | And (f1,f2) -> eval e f1 && eval e f2
  | All(p,f) -> let e' b = fun q -> if q <> p then e q else b
    in eval (e' true) f && eval (e' false) f 

let is_true (f : formula) = eval (fun _ -> failwith "free var") f
end



module NestedQBF = struct
  type 'v inc = Z | S of 'v
  type 'v formula =
  | Var of 'v
  | Bot
  | Not of 'v formula
  | And of 'v formula * 'v formula
  | All of 'v inc formula

  type 'v env = 'v -> bool
  let rec eval : type v. v env -> v formula -> bool = 
    fun env f ->
    match f with
    | Var(v) -> env v
    | Bot -> false
    | Not(f) -> not (eval env f)
    | And(f1,f2) -> eval env f1 && eval env f2
    | All(f) -> let env' b = function
      | Z  ->  b     (*tutaj to jest zmienna dopiero co zdefiniowana pod kwantyfikatorem, powinnismy tu na obie mozliwosci przypisac wartosc*)
      | S v -> env v (*to znaczy ze juz wczesniej bylo v gdzies definiowane*)
    in eval (env' true) f && eval (env' false) f
end
let absurd (x : empty) = match x with | _ -> .

open NestedQBF
let t1 = All(And(Var Z, All(And(Var (S Z), Var Z))))



let rec is_true (f : empty NestedQBF.formula) = NestedQBF.eval absurd f



type 'v env = QBF.var -> 'v
let rec scope_check : type v. v env -> QBF.formula -> v NestedQBF.formula =
    fun env f ->
    match f with
    | Var v -> Var(env v)
    | Bot -> Bot
    | Not f -> Not (scope_check env f)
    | And (f1,f2) -> And(scope_check env f1,scope_check env f2)
    | All(p, f) -> let env' = fun v -> if v = p then Z else S (env v)
      in All(scope_check env' f)

let convert f = scope_check (fun _  -> failwith "aaa") f
let f1 = let open QBF in All("p", And(Var"p", Not (Var "p")))
let f2 = let open QBF in All("p", And(Var"p", All ("q", And (Var "p", Var "q"))))

let f3 = let open QBF in All("p", And(Var"p", All ("q", And (Var "p", And (Var "r", Var "q")))))
let t1 = convert f1
let t2 = convert f2
let t3 = convert f3