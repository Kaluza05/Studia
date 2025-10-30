let is_generated (type a) (type p) (module P : Perm.S with type key = a and type t = p) (perm : p) (generators : p list) : bool =
  let 
  module SetP = Set.Make(struct
    type t = p
    let compare = P.compare
    end) 
  in

  let rec find_until_closure (generated : SetP.t) = 
    if SetP.mem perm generated then true else  
    let inverse = SetP.fold (fun p acc -> SetP.add (P.invert p) acc) generated SetP.empty in 
    let composed = SetP.fold 
    (fun p acc -> 
      (SetP.fold (fun p' acc' -> SetP.add (P.compose p p') acc' ) generated acc) ) 
    generated SetP.empty in 
    let new_generated = generated |> SetP.union inverse |> SetP.union composed in
    if SetP.compare generated new_generated = 0 then false
    else find_until_closure new_generated
  in 
  find_until_closure (SetP.of_list generators)
