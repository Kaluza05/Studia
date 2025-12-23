module GlobalSBT(State : sig type t end) : sig
  type 'a t
  val return : 'a -> 'a t
  val bind   : 'a t -> ('a -> 'b t) -> 'b t
  val fail : 'a t
  val flip : bool t
  val get : State.t t
  val put : State.t -> unit t
  val run : State.t -> 'a t -> 'a Seq.t
end = struct
  type 'a t = State.t -> 'a sbt_list * State.t
  and 'a sbt_list =
  | Nil
  | Cons of 'a * 'a t

  let rec concat (m1 : 'a t) (m2 : 'a t) : 'a t = 
    fun t -> let ls1,t = m1 t in  
    match ls1 with
    | Nil -> m2 t
    | Cons(a,r) -> Cons(a, concat r m2),t

  let return x = fun t -> Cons(x,fun s -> Nil, s), t
  let rec bind (m : 'a t) (f : 'a -> 'b t) : 'b t = fun t -> let ls,t = m t in 
      match ls with
      | Nil -> Nil,t
      | Cons(a,r) ->  concat (f a) (bind r f) t

  let fail =  fun s -> Nil, s
  let flip =  fun s -> Cons(true,fun s' -> Cons(false,fun s'' -> Nil,s''), s'),s
  let get = fun t -> Cons(t,fun s -> Nil,s), t
  let put t = fun _ -> Cons((), fun s -> Nil,s), t
  let rec run t m = 
    let s,t = m t in
    match s with
    | Nil -> Seq.empty
    | Cons(x,a) -> fun () -> Seq.Cons(x, run t a)
end 


module BTLoc  = GlobalSBT(Int)
open BTLoc

let (let*) = bind

let test1 =
  let prog =
    let* x =  flip in 
    let* y = flip in 
    let* _ = put 67 in 
    if y then 
    let* _ = put 100 in 
    let* s = get in  
    return (x,y,s) 
    else 
      let* s = get in
      return (x,y,s)
  in
  run 0 prog |> List.of_seq