type 'a crsr = 
  | Empty                          (*kursor na pustej liscie albo cos, albo kursor na końcu*)
  | Cons of 'a crsr * 'a           (*kursor, jak dojsc do tego miejsca, element*)

type 'a zlist = 'a crsr * 'a list   (*jak dojsc do tego miejsca, plus reszta listy*)


let of_list (ls : 'a list) : 'a zlist = Empty,ls

let to_list (crs,ls :'a zlist) : 'a list =
  let rec it crs acc = 
    match crs with
    | Empty -> acc
    | Cons(crs,x) -> it crs (x :: acc)
  in it crs ls

let elem (crs,ls :'a zlist)  : 'a option = 
  match ls with
  | []            -> None
  | x :: _ -> Some(x)

let move_left (crs,ls : 'a zlist) : 'a zlist  = 
  match crs with
  | Empty -> failwith "can't go farther left"
  | Cons(crs,a) -> crs, a :: ls

let move_right (crs,ls : 'a zlist) : 'a zlist  = 
  match ls with
  | [] -> failwith "can't go farther right"
  | x :: xs -> Cons(crs,x), xs 

let insert  (x :'a) (crs,ls : 'a zlist) : 'a zlist = Cons(crs,x),ls

let remove (crs,ls : 'a zlist) : 'a zlist  = 
    match crs with
    | Empty -> failwith "removing from empty list"
    | Cons(crs',a) -> 
        begin match crs' with
        | Empty -> failwith "trying to remove element before, while there is none before"
        | Cons(crs,_) -> Cons(crs,a) , ls
      end

  
let ls = [1;2;3;4;5]

let c_list = of_list ls;;
let c_list = insert 6 c_list;;
let c_list = move_left c_list;;