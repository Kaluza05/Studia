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
  type 'r ans = 'r option
  type 'a t = {run : 'r. ('a -> 'r ans) -> 'r ans}

  let return (x : 'a) = {run = fun cont -> cont x}
  let bind (m : 'a t) (f : 'a -> 'b t) : 'b t = 
    {run = fun cont -> m.run (fun a -> (f a).run cont )}

  let fail =  {run = fun cont -> None}
  let run (a : 'a t) = a.run (fun a -> Some(a)) 

  let catch (m : 'a t) (failed : unit -> 'a t) : 'a t = 
    {run = fun cont -> 
      match run m with
      | None -> (failed ()).run cont 
      | Some x -> cont x}
      
end


module BT : sig
  include Monad

  val fail : 'a t
  val flip : bool t

  val run : 'a t -> 'a Seq.t
end = struct
  type 'r ans = 'r Seq.t
  type 'a t = {run : 'r. ('a -> 'r ans) -> 'r ans}

  let return x = {run = fun cont -> cont x}
  let rec bind m f = {run = fun cont -> m.run (fun a -> (f a).run cont)}

  let fail = {run = fun cont -> Seq.empty}
  let flip = {run = fun cont -> Seq.append (cont true) (cont false)}
  let run m = m.run (fun a -> Seq.return a)
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
  (* mozna przerobić na T.t -> 'r, bo nie potrzebujemy stanu już w monadzie kontynuacyjnej *)
  type 'r ans = T.t -> T.t * 'r
  type 'a t = {run : 'r. ('a -> 'r ans) -> 'r ans}
  
  let return x = {run = fun cont -> cont x}
  let bind m f = {run = fun cont -> m.run (fun a -> (f a).run cont)}

  let get = {run = fun cont -> fun s -> cont s s}
  let put s = {run = fun cont _ -> cont () s}

  let run (s : T.t) (m : 'a t) = snd (m.run (fun a t -> t,a) s)
end



module St2(T : sig type t end) : sig
  include Monad

  val get : T.t t
  val put : T.t -> unit t

  val run : T.t -> 'a t -> 'a
end = struct
  (* mozna przerobić na T.t -> 'r, bo nie potrzebujemy stanu już w monadzie kontynuacyjnej *)
  type 'r ans = T.t -> 'r
  type 'a t = {run : 'r. ('a -> 'r ans) -> 'r ans}
  
  let return x = {run = fun cont -> cont x}
  let bind m f = {run = fun cont -> m.run (fun a -> (f a).run cont)}

  let get = {run = fun cont -> fun s -> cont s s}
  let put s = {run = fun cont _ -> cont () s}

  let run (s : T.t) (m : 'a t) = m.run (fun a t -> a) s
end
