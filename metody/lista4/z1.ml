module type DICT = sig
  type ('a , 'b ) dict
  val empty : ('a , 'b ) dict
  val insert : 'a -> 'b -> ('a , 'b ) dict -> ('a , 'b ) dict
  val remove : 'a -> ('a , 'b ) dict -> ('a , 'b ) dict
  val find_opt : 'a -> ('a , 'b ) dict -> 'b option
  val find : 'a -> ('a , 'b ) dict -> 'b
  val to_list : ('a , 'b ) dict -> ('a * 'b ) list
  end

module ListDict : DICT = struct
  type ('a,'b) dict = ('a * 'b) list
  let empty = []
  let insert x y list = (x,y) :: list
  let rec remove x list = 
    match list with
    | [] -> empty
    | (y,b)::ys' -> if x = y then ys' else (y,b) :: remove x ys'
  let rec find_opt x list = 
    match list with
    | [] -> None
    | (y,b)::ys' -> if x = y then Some b else find_opt x ys'

  let rec find x list = 
    match list with
    | [] -> failwith "nie ma elementu"
    | (y,b)::ys' -> if x = y then b else find x ys'
  let to_list list : ('a * 'b) list= list
end

