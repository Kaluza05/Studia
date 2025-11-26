module type Monad = sig
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end


module Err : sig
  include Monad
  val fail : 'a t
  val catch : 'a t -> (unit -> 'a t) -> 'a t
  val run : 'a t -> 'a option
end = struct
  type 'r ans = 'r
  type 'a t = {run : 'r. ('a option -> 'r ans) -> 'r ans}

  let return (x : 'a) = {run = fun cont -> cont (Some x)}
  let bind (m : 'a t) (f : 'a -> 'b t) : 'b t = 
    {run = fun cont -> m.run (fun a ->
      match a with
      | None   ->   cont None
      | Some(a)->  (f a).run cont)}


  let fail =  {run = fun cont -> cont None}
  let catch a failed = 
    {run = fun cont -> a.run 
    (fun a -> 
      match a with
      | None -> (failed ()).run cont 
      | Some a -> cont (Some a))}
      
  let run (a : 'a t) = a.run (fun a -> a)
end


module BT : sig
  include Monad

  val fail : 'a t
  val flip : bool t

  val run : 'a t -> 'a Seq.t
end = struct
  type 'r ans = 'r
  type 'a t = {run : 'r. ('a Seq.t -> 'r ans) -> 'r ans}

  let return x = {run = fun cont -> cont (Seq.return x)}
  let rec bind m f = 
    {run = fun cont -> 
      m.run (fun a -> cont (Seq.concat_map (fun a -> (f a).run (fun c' -> c')) a))}

  let fail = {run = fun cont -> cont (Seq.empty)}
  let flip = {run = fun cont -> cont (List.to_seq [true; false])}
  let run m = m.run (fun a -> a)
end

open BT
let (let*) = bind

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

let example =
  let* b1 =  flip in
  let* b2 =  flip in 
  return (b1, b2)

let results = run example |> List.of_seq
(* results = [(true,true); (true,false); (false,true); (false,false)] *)


module St(T : sig type t end) : sig
  include Monad

  val get : T.t t
  val put : T.t -> unit t

  val run : T.t -> 'a t -> 'a
end = struct
  type 'r ans = T.t * 'r
  type 'a t = {run : 'r. ((T.t -> T.t * 'a) -> 'r ans) -> 'r ans}
  (* {run : } *)
  let return x = {run = fun cont -> cont (fun s -> s,x)}
  let bind m f = 
    {run = fun cont -> 
      m.run (fun s -> fun s -> let t',a = s t in failwith "a")}

  let get = {run = fun cont -> cont (fun s -> s,s)}
  let put s = {run = fun cont -> cont (fun _ -> s,() )}

  let run s m = snd (m.run (fun cont ->  cont s))
end
