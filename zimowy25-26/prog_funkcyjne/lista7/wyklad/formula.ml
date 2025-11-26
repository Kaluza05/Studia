type var = string

type formula =
  | Top
  | Var of var
  | Not of formula
  | And of formula * formula

module type S = sig
  type ans

  type 'a t

  val return : 'a -> 'a t
  val bind   : 'a t -> ('a -> 'b t) -> 'b t

  val lookup : var -> bool t

  val exists : bool t -> bool
  val forall : bool t -> bool
end

module Env : S = struct
  module VarMap = Map.Make(String)

  type ans = bool VarMap.t -> bool

  type 'a t = ('a -> ans) -> ans

  let return x cont = cont x
  let bind m f cont =
    m (fun x -> f x cont)

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
  let (let* ) = Env.bind
  (* let* x = e1 in e2  ---> (let* ) e1 (fun x -> e2) *)

  let rec eval f =
    match f with
    | Top   -> Env.return true
    | Var x -> Env.lookup x
    | Not f ->
      let* b = eval f in
      Env.return (not b)
    | And(f1, f2) ->
      let* b1 = eval f1 in
      if b1 then eval f2
      else Env.return false

  let sat   f = Env.exists (eval f)
  let tauto f = Env.forall (eval f)
end

module M = F(Env)
include M
