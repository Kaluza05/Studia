open Syntax
module StrMap = Map.Make(String)

exception TermFormatError    of position * string
exception FormulaFormatError of position * string
exception VariableUsed       of position * string
exception FreeVariable       of position * string
exception WrongType          of position * string * string * string
exception WrongFormula       of position * string * string * string
exception TheoremError       of position * string * string

(** infix notation for Formula.equal *)
let ( <=> ) (f1 : formula) (f2 : formula) = Formula.equal f1 f2

let check_var_env (x : string) (env : unit StrMap.t) (pos : position) : unit =
  if StrMap.mem x env then raise (VariableUsed (pos, x)) else ()

let rec wft (t : term) (env : unit StrMap.t) (pos : position) : unit =
  match t with
  | Var t -> if StrMap.mem t env then () else raise (TermFormatError (pos, t))
  | Func (_, ts) -> List.iter (fun t -> wft t env pos) ts

let wff (f : formula) (env : unit StrMap.t) (pos : position) : unit =
    let rec it (g : formula) (env : unit StrMap.t) : unit =
    try
      match g with
      | False                                     -> ()
      | Rel (_r, ts)                              -> List.iter (fun t -> wft t env pos) ts
      | Imp (f1, f2) | And (f1, f2) | Or (f1, f2) -> it f1 env; it f2 env
      | Forall (x, f) | Exists (x, f)             -> it f (StrMap.add x () env)
      | ForallRel (_r, f)                         -> it f env
    with
    | TermFormatError (_, t) -> raise (
        FormulaFormatError (pos, Printf.sprintf "%s inside: %s" t (Formula.to_string f)))
    | FormulaFormatError (_, mess) -> raise (FormulaFormatError (pos, mess))
  in
  it f env

let rec infer_proof_type (e : expr) (vars : unit StrMap.t) (assumptions : formula StrMap.t) : formula =
  let pos = e.pos in
  match e.data with
  | EVar x -> (
        match StrMap.find_opt x assumptions with
        | Some f -> f
        | None -> raise (FreeVariable (pos, x)))
  | ELet (x, e1, e2) -> 
        let f1 = infer_proof_type e1 vars assumptions in 
        infer_proof_type e2 vars (StrMap.add x f1 assumptions)
  | EFun (x, f, e) ->
        wff f vars pos;
        let y = infer_proof_type e vars (StrMap.add x f assumptions) in
        Imp (f, y)
  | EApp (e1, e2) -> (
        match infer_proof_type e1 vars assumptions with
        | Imp (f1, f2) ->
            let f3 = infer_proof_type e2 vars assumptions in
            if f1 <=> f3 then f2
            else 
                raise(WrongFormula 
                (pos, "app", Formula.to_string f1, Formula.to_string f3))
        | f -> raise (WrongType (pos, "app", Formula.to_string f, "φ → ψ")))
  | ETermFun (x, e) ->
        check_var_env x vars pos;
        Forall (x, infer_proof_type e (StrMap.add x () vars) assumptions)
  | ETermApp (e, t) -> (
        wft t vars pos;
        match infer_proof_type e vars assumptions with
        | Forall (x, f) -> Formula.subst x t f
        | f -> raise (WrongType (pos, "term app", Formula.to_string f, "∀ x, φ")))
  | ERelApp (e, x, f1) -> (
        wff f1 (StrMap.add x () vars) pos;
        match infer_proof_type e vars assumptions with
        | ForallRel (r, f2) -> Formula.subst_rel r (x, f1) f2
        | f -> raise (WrongType (pos, "rel app", Formula.to_string f, "∀ {R}, φ"))
        )
  | EPair (e1, e2) ->
        And
        (infer_proof_type e1 vars assumptions,
         infer_proof_type e2 vars assumptions )
  | EFst e -> (
        match infer_proof_type e vars assumptions with
        | And (f1, _) -> f1
        | f -> raise (WrongType (pos, "fst", Formula.to_string f, "φ ∧ ψ")))
  | ESnd e -> (
        match infer_proof_type e vars assumptions with
        | And (_, f2) -> f2
        | f -> raise (WrongType (pos, "snd", Formula.to_string f, "φ ∧ ψ")))
  | ELeft (e1, f) -> (
        wff f vars pos;
        match f with
        | Or (f1, _) ->
            let f2 = infer_proof_type e1 vars assumptions in
            if f1 <=> f2 then f
            else 
            raise(WrongType
            (pos, "left", Formula.to_string f2, Formula.to_string f1))
        | f -> raise (WrongFormula (pos, "left", Formula.to_string f, "φ ∨ ψ")))
  | ERight (e1, f) -> (
        wff f vars pos;
        match f with
        | Or (_, f2) ->
            let f1 = infer_proof_type e1 vars assumptions in
            if f1 <=> f2 then f
            else
            raise(WrongType
            (pos, "right", Formula.to_string f1, Formula.to_string f2))
        | f -> raise (WrongFormula (pos, "left", Formula.to_string f, "φ ∨ ψ")))
  | ECase (e, n1, e1, n2, e2) -> (
        match infer_proof_type e vars assumptions with
        | Or (f1, f2) ->
            let fi1, fi2 =
              infer_proof_type e1 vars (StrMap.add n1 f1 assumptions),
              infer_proof_type e2 vars (StrMap.add n2 f2 assumptions)
            in
            if fi1 <=> fi2 then fi1
            else
            raise(WrongType
            (pos, "case", Formula.to_string fi1, Formula.to_string fi2))
        | f -> raise (WrongFormula (pos, "left", Formula.to_string f, "φ ∨ ψ")))
    | EAbsurd (e, f) -> (
        wff f vars pos;
        match infer_proof_type e vars assumptions with
        | False -> f
        | f -> raise (WrongType (pos, "absurd", Formula.to_string f, "False")))
  | EPack (t, e, f) -> (
        wft t vars pos;
        wff f vars pos;
        match f with
        | Exists (x, f1) ->
            let f2 = infer_proof_type e vars assumptions in
            if  (Formula.subst x t f1) <=> f2 then f
            else
            raise(WrongType
            (pos, "pack", Formula.to_string f1, Formula.to_string f2))
        | _ -> raise (WrongFormula (pos, "pack", Formula.to_string f, "∃ x, φ")))
  | EUnpack (x, y, e1, e2) ->
        (match infer_proof_type e1 vars assumptions with
        | Exists (x1, f) ->
            check_var_env x vars pos;
            let f1 = 
                infer_proof_type e2 (StrMap.add x () vars)
                (StrMap.add y (Formula.subst x1 (Var(x)) f) assumptions)
            in
            wff f1 vars pos;
            f1
        | f -> raise (WrongFormula (pos, "unpack", Formula.to_string f, "∃ x, f"))
        )

let check_defs (defs : def list) : unit =
  let rec it (defs : def list) (given : formula StrMap.t) : unit =
    match defs with
    | [] -> ()
    | Axiom (pos, name, axiom) :: xs' ->
        (try wff axiom (StrMap.empty) pos
        with
        | TermFormatError (pos, t)    -> raise (Syntax.Type_error (pos, "term not formated: "    ^ t ))
        | FormulaFormatError (pos, f) -> raise (Syntax.Type_error (pos, "formula not formated: " ^ f ))
        | _ -> raise (Syntax.Type_error (pos, "unexpected exception")));
        it xs' (StrMap.add name axiom given)
    | Theorem (pos, name, theorem, proof) :: xs' -> (
        try
            wff theorem (StrMap.empty) pos;
            let proof_type = infer_proof_type proof StrMap.empty given in
            if  proof_type <=> theorem then ()
            else
            raise(TheoremError
            (pos, Formula.to_string theorem, Formula.to_string proof_type))
        with
        | VariableUsed (pos, x)       -> raise (Syntax.Type_error (pos, "Variable used: "        ^ x ))
        | FreeVariable (pos, x)       -> raise (Syntax.Type_error (pos, "Free variable: "        ^ x ))
        | TermFormatError (pos, t)    -> raise (Syntax.Type_error (pos, "Term not formated: "    ^ t ))
        | FormulaFormatError (pos, f) -> raise (Syntax.Type_error (pos, "Formula not formated: " ^ f ))
        | WrongFormula (pos, e, got, wanted) ->
            let mess = Printf.sprintf "Wrong formula in %s:\nwanted: %s\ngot: %s" e wanted got
            in raise (Syntax.Type_error (pos, mess))
        | WrongType (pos, e, got, wanted) ->
            let mess = Printf.sprintf "Wrong infer in %s:\nwanted: %s\ngot: %s" e wanted got
            in raise (Syntax.Type_error (pos, mess))
        | TheoremError (pos, theorem, proof) ->
            let mess = Printf.sprintf "Wrong proof: %s\n for theorem: %s" proof theorem
            in raise (Syntax.Type_error (pos, mess))
        | _ -> raise (Syntax.Type_error (pos, "Unexpected exception")));
        it xs' (StrMap.add name theorem given)
  in
  it defs StrMap.empty

