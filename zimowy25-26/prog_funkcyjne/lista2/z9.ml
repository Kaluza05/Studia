type 'a clist = { clist : 'z. ('a -> 'z -> 'z) -> 'z -> 'z }

let cnil = {clist = fun _ z -> z}
let ccons (a : 'a) (ls : 'a clist) = {clist = fun f z -> f a (ls.clist f z)} 
let map (g : 'a -> 'b)  (ls : 'a clist) : 'b clist = {clist = fun f z -> ls.clist (fun a z -> f (g a) z) z}
let append (ls1 : 'a clist) (ls2 : 'a clist) : 'a clist= 
  {clist = fun f z -> ls1.clist f (ls2.clist f z)}

let prod (ls1: 'a clist) (ls2 : 'b clist) = 
  { clist = fun f z -> ls1.clist (fun a acc -> ls2.clist (fun b acc' -> f (a, b) acc') acc) z}

let clist_to_list (ls : 'a clist) : 'a list = 
  ls.clist (fun a z -> a :: z) []
let clist_of_list ( xs : 'a list) : 'a clist = 
  {clist = fun f z -> List.fold_right f xs z}
let rec clist_of_list2 ( xs : 'a list) : 'a clist = 
  match xs with 
  | [] -> cnil
  | x :: xs' -> ccons x (clist_of_list2 xs')
  

let ls = [1;2;3;4]
let ls2 = [7;8;9]
let cls = clist_of_list ls
let cls2 = clist_of_list ls2

let test1 = clist_to_list (append cls cls2)
let test2 = clist_to_list (map (fun a -> a*a) cls)
let test3 = clist_to_list (ccons 11 cls)