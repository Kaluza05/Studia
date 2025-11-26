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
    lazy {prev =  (of_seqs x1 (s1' ()) (Cons(x, fun () -> s2)));
    elem = x;
    next =  (of_seqs x2 (Cons(x, fun () -> s1)) (s2' ()))}



let int_list = 
  let pos_ints = (Seq.ints 1) in 
  lazy (of_seqs 0 (Seq.map (fun i -> -i )  pos_ints ()) (pos_ints ()))



(*other implementation without branching*)

(*s1 is going to be turned into prev
  s2 into next
  
  mem1 is memory of previous prev
  mem2 is memory of previous next

  if mem_i is not None that means we dont have to compute it again, as that would make a new object and branch
  *)

let from_seq x s1 s2 = 
  let open Seq in
  let rec it x s1 s2 mem1 mem2 =
    match s1, s2 with
    | Nil, _ | _, Nil -> failwith "empty seq"
    | Cons(x',s1'), Cons(x'',s2') -> 

      begin match mem1,mem2 with
      | Some(p), Some(n) ->
        lazy {
          prev = p;
          elem = x;
          next = n}

      | Some(p), None -> 
        let mem_prev = Cons(x,fun () -> s1) in
        let rec self = 
        lazy {
          prev = p;                                  (*we know p so we use it*)
          elem = x;
          next = it x'' mem_prev (s2' ()) (Some self) None}    (*now in the empty hole we should put Some(the object we just calculated)*)
        in self

      | None, Some(n) ->
        let mem_next = Cons(x,fun () -> s2) in
        let rec self = 
        lazy {
          prev = it x'  (s1' ()) mem_next None (Some self);
          elem = x;
          next = n}
        in self

      | None, None -> 
        let mem_next = Cons(x,fun () -> s2) in
        let mem_prev = Cons(x,fun () -> s1) in
        let rec self = 
        lazy {
          prev = it x'  (s1' ()) mem_next None (Some self);
          elem = x;
          next = it x'' mem_prev (s2' ()) (Some self) None}
        in self

      end

  in it x s1 s2 None None

let of_list (ls : 'a list) = 
  match ls with
  | [] -> failwith "should be empty"
  | x :: xs' -> 
    let ls' = xs' @ [x] in 
    let rev_req = ls |> List.rev |> List.to_seq |> Seq.cycle in
    let ls_seq = ls'             |> List.to_seq |> Seq.cycle in
    from_seq x (ls_seq ()) (rev_req ())


let int_list = 
  let pos_ints = (Seq.ints 1) in 
  (from_seq 0 (Seq.map (fun i -> -i )  pos_ints ()) (pos_ints ()))

(*
int_list == int_list |> prev |> next
int_list == int_list |> next |> prev
int_list == (int_list |> prev |> prev |> next |> next |> prev |> next)
*)

let cycle s = 
  match s () with
  | Seq.Nil ->  failwith "daaaa"
  | _ ->
  let rec it s' = 
    match s' () with
    | Seq.Nil -> it s
    | Seq.Cons(x,s') -> Seq.Cons(x,fun () -> it s')

  in it s