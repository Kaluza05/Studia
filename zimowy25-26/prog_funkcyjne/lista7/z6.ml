module  LocalSBT(State : sig type t end) : sig
  type 'a t
  val return : 'a -> 'a t
  val bind   : 'a t -> ('a -> 'b t) -> 'b t
  val fail : 'a t
  val flip : bool t
  val get : State.t t
  val put : State.t -> unit t
  val run : State.t -> 'a t -> 'a Seq.t
end = struct

  type 'a t = State.t -> ('a * State.t) Seq.t
  let return x = fun t -> Seq.return (x,t)
  let bind (m : 'a t) (f : 'a -> 'b t) : 'b t = 
    fun t -> Seq.flat_map (fun (x,s) -> f x s) (m t)
  let fail = fun _ -> Seq.empty
  let flip = fun s -> List.to_seq [(true, s);(false, s)]
  let get = fun t -> Seq.return (t,t)
  let put t = fun _ -> Seq.return ((), t)
  let run (t : State.t) (m : 'a t) : 'a Seq.t = Seq.map fst (m t)
end