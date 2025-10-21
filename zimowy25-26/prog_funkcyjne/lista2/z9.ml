type 'a clist = { clist : 'z. ('a -> 'z -> 'z) -> 'z -> 'z }

let cnil = {clist = fun _ z -> z}
let ccons (a : 'a) (ls : 'a clist) = {clist = fun f z -> f a (ls.clist f z)} 
let map (g : 'a -> 'b)  (ls : 'a clist) : 'b clist = {clist = fun f z -> ls.clist (fun a z -> f (g a) z) z}
let append (ls1 : 'a clist) (ls2 : 'a clist) : 'a clist= 
  {clist = fun f z -> ls1.clist f (ls2.clist f z)}
let clist_to_list (ls : 'a clist) : 'a list = 
  ls.clist (fun a z -> a :: z) []
let clist_of_list ( xs : 'a list) : 'a clist = 
  {clist = fun f z ->
  let rec curry xs = 
    match xs with
    | [] -> z
    | x :: xs' -> f x (curry xs')
  in curry xs}

let rec clist_of_list2 ( xs : 'a list) : 'a clist = 
  match xs with 
  | [] -> cnil
  | x :: xs' -> ccons x (clist_of_list2 xs')
  