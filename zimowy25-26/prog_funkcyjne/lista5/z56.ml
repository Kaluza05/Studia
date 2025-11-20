type 'a dllist = 'a dllist_data lazy_t
and 'a dllist_data =
  { prev : 'a dllist;
    elem : 'a;
    next : 'a dllist
  }

let prev (lazy dls : 'a dllist) : 'a dllist = dls.prev

let elem (lazy dls : 'a dllist) : 'a        = dls.elem

let next (lazy dls : 'a dllist) : 'a dllist = dls.next

(* dwukierunkowa cykliczna, jakoś zrobic ze tworzymy od ls[0] i pozniej tworzymy dla ls[1:] i jak bedzie pusta do znowu ls
[1,2,3] ->  prev :    3    1   2    {3}   1    2    3    1    2    3
            elem :    3                  {1}
            next :    3    1   2     3    1   {2}   3    1    2    3*)

(* wiec bysmy chcieli cos w stylu
prev = centered_at 3
elem = 1 
next = centered at 2*)

(*

make 1 [2;3] [3;2;1]
-> prev = make 3 [1;2;3] [2;1]
-> next = make 2 [3] [1:3:2:1]


pozniej 
make 3 [] [2:1:3:2:1]  == prev normalnie, next = make l ls' ls_rev
prev = make 2 [3] [1:3:2:1]
next = 1 [2:3] [3:2:1]

make 1 [2:3;1;2;3] [] == next normalnie, prev = make l ls' ls_rev

prev = make 3 [1:2:3] [2:1]
next = make 2 [3:1:2:3] [1]

*)
let of_list (ls : 'a list) = 
  let ls_rev = List.rev ls in
  match ls, ls_rev with
  | [],_ | _,[]   -> failwith "empty list"
  | l :: ls', l' :: ls_rev' ->
    let rec make x xs ys = 
      match xs, ys with
      | [] , [] -> failwith "impossible case"
      | [], y' :: ys' -> 
        lazy {prev = make y' [x] ys';
        elem = x;
        next = make l ls' ls_rev}
      | x' :: xs', [] ->
        lazy {prev = make l' ls ls_rev';
        elem = x;
        next = make x' xs' [x]}
      | x' :: xs', y' :: ys' -> 
        lazy {prev = make y' (x :: xs) ys';
        elem = x;
        next = make x' (xs') (x :: ys)}

    in make l ls' ls_rev


let rec of_seqs x s1 s2 =
  let open Seq in
  match s1, s2 with
  | Nil,_ | _, Nil -> failwith "shouldn't be empty"
  | Cons(x1,s1'), Cons(x2,s2') -> 
    {prev = lazy (of_seqs x1 (s1' ()) (Cons(x, fun () -> s2)));
    elem = x;
    next = lazy (of_seqs x2 (Cons(x, fun () -> s1)) (s2' ()))}

(* zeby sie nie rozwarstwaiala nalezy zrobic zeby trzymala fizycznie poprzednie obiekty 
  listy można zamienić na seq, i zrobić cykliczny seq*)

let int_list = 
  let pos_ints = (Seq.ints 1) in 
  lazy (of_seqs 0 (Seq.map (fun i -> -i )  pos_ints ()) (pos_ints ()))
