 type ty_name = string
 type ctor_name = string
 type ctor = Ctor of ctor_name * ty_name list
 type ty_def = ty_name * ctor list

let a : ty_def =("int_list",
 [ Ctor ("Nil", []);
 Ctor ("Cons", ["int"; "int_list"])
 ])

let b : ty_def =("'a tree",
 [ Ctor ("Leaf", []);
 Ctor ("'a Leaf",["'a"]);
 Ctor ("Node", ["'a tree"; "'a";"'a tree"])
 ])

let mk_ind (ty : ty_def) = 
  let t, cons_lst = ty in
  let lbl = ref 0 in
  let lbl_counter () = 
    lbl := !lbl + 1 
  in
  let rec cons_rules cons_lst = 
    match cons_lst with
    | [] -> ()
    | Ctor(name,types) :: xs -> 
      if types = [] then (Printf.printf "P(%s) =>" name;
      cons_rules xs)
      else
      let rec mk_imp types vars recur imp = 
        match types with
        | [] -> vars,recur,imp
        | x :: xs -> 
          let new_var = Printf.sprintf "x%d" !lbl in 
          let new_vars = Printf.sprintf "%s : %s, " new_var x in
          let new_imp = Printf.sprintf "%s, " new_var in 
          lbl_counter ();
          if x = t then 
            let new_recur = Printf.sprintf "P(%s) => " new_var in
            mk_imp xs (vars ^ new_vars) (recur ^ new_recur) (imp ^ new_imp)
        else mk_imp xs (vars ^ new_vars) (recur) (imp ^ new_imp)
        in
      let vars,recur,imp = mk_imp types "" "" "" in
      Printf.printf "\n∀ %s : %s P(%s(%s)) => " vars recur name imp;
      cons_rules xs;
      
    in
  Printf.printf "Induction for type: %s \n" t;
  Printf.printf "∀ x : %s\n" t;
  cons_rules cons_lst;
  Printf.printf "\nP(x)";