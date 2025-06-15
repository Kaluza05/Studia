type ident = string

type qbf =
  | Top                   (* ⊤ *)
  | Bot                   (* ⊥ *)
  | Not    of qbf         (* ¬ *)
  | Var    of ident       (* x *)
  | Disj   of qbf * qbf   (* ∨ *)
  | Conj   of qbf * qbf   (* ∧ *)
  | Forall of ident * qbf (* ∀ *)
  | Exists of ident * qbf (* ∃ *)
