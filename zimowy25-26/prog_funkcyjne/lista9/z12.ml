module type Monad = sig
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

module BT : sig
  include Monad
  val fail : 'a t
  val flip : bool t
  val run : 'a t -> 'a Seq.t
end = struct
  type 'a t = 'a Seq.t
  let return x = Seq.return x
  let bind m f = Seq.concat_map f m
  let fail = Seq.empty
  let flip = List.to_seq [true;false]
  let run m = m
end

type empty = |

type _ fin_type =
| Empty : empty fin_type
| Either : 'a fin_type * 'b fin_type -> ('a, 'b) Either.t fin_type
| Unit : unit fin_type
| Bool : bool fin_type
| Pair : 'a fin_type * 'b fin_type -> ('a * 'b) fin_type


let all_values (f : 'a fin_type)  : 'a Seq.t = 
  let open BT in
  let (let*) = bind in
  let rec it : type a. a fin_type -> a BT.t = function
    | Empty -> fail
    | Either(f1,f2) -> 
      let* b = flip in 
      if b then 
        let* f1 = it f1 in
        return (Either.Left f1) 
      else 
        let* f2 = it f2 in 
        return (Either.Right f2)
    | Unit -> return ()
    | Bool -> flip
    | Pair(f1,f2) -> 
      let* r1 = it f1 in
      let* r2 = it f2 in
      return (r1,r2)

  in it f |> run


let t1 = all_values (Pair(Unit,Bool)) |> List.of_seq
let t2 = all_values (Pair(Either(Bool,Empty),Bool)) |> List.of_seq
