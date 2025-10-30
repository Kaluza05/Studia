module type OrderedType = sig
    type t
    val compare : t -> t -> int
    end

module type S = sig
    type key
    type t
    (** permutacja jako funkcja *)
    val apply : t -> key -> key
    (** permutacja identycznościowa *)
    val id : t
    (** permutacja odwrotna *)
    val invert : t -> t
    (** permutacja która tylko zamienia dwa elementy miejscami *)
    val swap : key -> key -> t
    (** złożenie permutacji (jako złożenie funkcji) *)
    val compose : t -> t -> t
    (** porównywanie permutacji *)
    val compare : t -> t -> int
    end

module Make(Key : OrderedType) : S with type key = Key.t = struct
  module M = Map.Make(Key)

  type key = Key.t
  type t = key M.t * key M.t

  let apply (perm, _ : t) v = 
    match M.find_opt v perm with
    | None -> v
    | Some(v) -> v

  let id = (M.empty, M.empty) 
  let invert (perm,inv : t) : t = (inv, perm)
  let swap v1 v2 : t =
    let perm = M.(empty |> add v1 v2 |> add v2 v1) in
    (perm, perm)


  let compose_maps m1 m2 = M.merge (fun _ m1_opt m2_opt ->
      match m2_opt with
        | Some y -> M.find_opt y m1
        | None -> 
          begin match m1_opt with
          | Some y -> Some(y)
          | None -> None
          end) 
          m1 m2

  let compose (p1,inv1) (p2, inv2) = 
    let perm = compose_maps p1 p2 in
    let inv = compose_maps inv2 inv1 in
    perm,inv
  let compare (t1,_) (t2,_) = M.compare Key.compare t1 t2
end



