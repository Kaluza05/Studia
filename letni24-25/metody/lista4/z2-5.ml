module type KDICT = sig
  type key
  type 'a dict
  val empty : 'a dict
  val insert : key -> 'a -> 'a dict -> 'a dict
  val remove : key -> 'a dict -> 'a dict
  val find_opt : key -> 'a dict -> 'a option
  val find : key -> 'a dict -> 'a
  val to_list : 'a dict -> (key * 'a) list
end

module MakeListDict (Ord : Map.OrderedType) : KDICT with type key = Ord.t = struct
  type key = Ord.t
  type 'a dict = (key * 'a) list
  let empty = []
  

  let rec remove x list = 
    match list with
    | [] -> empty
    | (y,b)::ys' -> if Ord.compare x y = 0 then ys' else (y,b) :: remove x ys'

  let insert x y list = (x,y) :: remove x list
    
  let rec find_opt x list = 
    match list with
    | [] -> None
    | (y,b)::ys' -> if Ord.compare x y = 0 then Some b else find_opt x ys'

  let rec find x list =
    match list with
    | [] -> failwith "nie ma w slowniku"
    | (y,b)::ys' -> if Ord.compare x y = 0 then b else find x ys'

  let  to_list list : (key * 'a) list= list
  end

module  CharKey = struct
  type t = char
  let compare = Char.compare
  
end

module CharListDict = MakeListDict (CharKey)

module MakeMapDict (Ord: Map.OrderedType) : KDICT with type key = Ord.t = struct
  type key = Ord.t
  module M = Map.Make(Ord)
  type 'a dict = 'a M.t
  let empty = M.empty
  let insert x y map = M.add x y map
  let remove x map = M.remove x map
  let find_opt x map = M.find_opt x map
  let find x map = M.find x map
  let to_list map  = M.bindings map
end

module CharMapDict = MakeMapDict(CharKey)


module Freq (D:KDICT) = struct
  let freq  (xs : D.key list) : (D.key * int) list =
    let rec it xs map = 
      match xs with 
      | [] -> map
      | x::xs' -> 
        let curr  = match D.find_opt x map with 
          | None -> 0
          | Some count -> count
        in it xs' (D.insert x (curr+1) map)
    in it xs D.empty |> D.to_list
end


let char_freq (xs : string) = 
  let module F = Freq(CharMapDict) 
  in xs |> String.to_seq |> List.of_seq |> F.freq


let char_freq2 (module D:KDICT with type key = char) (xs : string) = 
  let freq  (xs : D.key list) : (D.key * int) list =
    let rec it xs map = 
      match xs with 
      | [] -> map
      | x::xs' -> 
        let curr  = match D.find_opt x map with 
          | None -> 0
          | Some count -> count
        in it xs' (D.insert x (curr+1) map)
    in it xs D.empty |> D.to_list
  in xs |> String.to_seq |> List.of_seq |> freq
