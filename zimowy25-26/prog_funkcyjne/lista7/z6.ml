module type SBT = functor (State : sig type t end) -> sig
  type 'a t
  val return : 'a -> 'a t
  val bind   : 'a t -> ('a -> 'b t) -> 'b t
  val fail : 'a t
  val flip : bool t
  val get : State.t t
  val put : State.t -> unit t
  val run : State.t -> 'a t -> 'a Seq.t
end

module GlobalBT : SBT = functor (State : sig type t end) -> struct

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

 module LocalBT : SBT = functor (State : sig type t end) -> struct
  type 'a t = State.t -> 'a sbt_list * State.t
  and 'a sbt_list =
  | Nil
  | Cons of 'a * 'a t

  let rec concat ls1 ls2 t = 
    match ls1 with
    | Nil -> ls2
    | Cons(x,r) -> let  ls1, t = r t in Cons(x, fun s -> concat ls1 ls2 t, s)
  let rec sbt_flat_map (a : 'a t) (f: 'a -> 'b t) (t : State.t) : 'b sbt_list = 
    let ls, t = a t in
    match ls with
    | Nil -> Nil
    | Cons(x,r) -> let ls, t = r t in concat (f x) (sbt_flat_map)   
  let return x = fun t -> Cons(x,fun s -> Nil, s), t
  let bind m f = fun t -> sbt_flat_map m f t , t
    (* napisać concat map na 'a sbt_list *)
  let fail =  fun s -> Nil, s
  let flip =  failwith "a"
  let get = fun t -> Nil, t (*albo moze Cont(t,Nil), t ale nie wiem czy to ma wiekszy sens, chyba tak*)
  let put t = fun _ -> Nil, t
  let rec run t a = let s,t = a t in
    match s with
    | Nil -> Seq.empty
    | Cons(x,a) -> fun () -> Seq.Cons(x, run t a)
end 


module BTGlob = GlobalBT(Int)
module BTLoc  = LocalBT(Int)
