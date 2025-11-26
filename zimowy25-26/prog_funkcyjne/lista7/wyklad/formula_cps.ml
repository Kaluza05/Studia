type var = string

type formula =
  | Top
  | Var of var
  | Not of formula
  | And of formula * formula

module type S = sig
  type ans

  type 'a t = ('a -> ans) -> ans

  val lookup : var -> bool t

  val exists : bool t -> bool
  val forall : bool t -> bool
end

module Env : S = struct
  module VarMap = Map.Make(String)

  type ans = bool VarMap.t -> bool

  type 'a t = ('a -> ans) -> ans

  let lookup x cont env =
    match VarMap.find_opt x env with
    | Some v -> cont v env
    | None   ->
      cont true  (VarMap.add x true  env) ||
      cont false (VarMap.add x false env)

  let exists c = c (fun b _ -> b) VarMap.empty
  let forall c = not (exists (fun cont -> c (fun b -> cont (not b))))
end

module F(Env : S) = struct
  let rec eval f cont =
    match f with
    | Top   -> cont true
    | Var x -> Env.lookup x cont
    | Not f -> eval f (fun b -> cont (not b))
    | And(f1, f2) ->
      eval f1 (fun b1 ->
      if b1 then eval f2 cont
      else cont false)

  let sat   f = Env.exists (eval f)
  let tauto f = Env.forall (eval f)
end

module M = F(Env)
include M
