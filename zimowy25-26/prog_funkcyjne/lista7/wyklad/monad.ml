module type Monad = sig
  type 'a t

  val return : 'a -> 'a t
  val bind   : 'a t -> ('a -> 'b t) -> 'b t

  (*
  let* x = return e1 in e2  ===  let x = e1 in e2

  let* x = e1 in return x  ===  e1

  let* x = let* y = e1 in e2 in e3  ===
  let* y = e1 in let* x = e2 in e3
  *)
end

module Err : sig
  include Monad

  val fail : 'a t

  val catch : 'a t -> (unit -> 'a t) -> 'a t

  val run : 'a t -> 'a option
end = struct
  type 'a t = 'a option

  let return x = Some x
  let bind m f =
    match m with
    | None   -> None
    | Some x -> f x

  let fail = None

  let catch m f =
    match m with
    | None   -> f ()
    | Some x -> Some x

  let run m = m
end

module BT : sig
  include Monad

  val fail : 'a t
  val flip : bool t

  val run : 'a t -> 'a Seq.t
end = struct
  type 'a t = 'a Seq.t

  let return x = Seq.return x
  let rec bind m f = Seq.concat_map f m

  let fail = Seq.empty
  let flip = List.to_seq [ true; false ]

  let run m = m
end

let (let* ) = BT.bind

let rec select a b =
  if a >= b then BT.fail
  else
    let* c = BT.flip in
    if c then BT.return a
    else select (a+1) b

let triples n =
  let* a = select 1 n in
  let* b = select a n in
  let* c = select b n in
  if a*a + b*b = c*c then BT.return (a, b, c)
  else BT.fail

module St(T : sig type t end) : sig
  include Monad

  val get : T.t t
  val put : T.t -> unit t

  val run : T.t -> 'a t -> 'a
end = struct
  type 'a t = T.t -> 'a * T.t

  let return x s = (x, s)
  let bind m f s =
    let (x, s) = m s in
    f x s

  let get s = (s, s)
  let put s _ = ((), s)

  let run s m = fst (m s)
end
