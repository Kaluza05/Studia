type formula = 
  | Bot
  | Var of string 
  | Imp of formula * formula

let rec string_of_formula f =
  match f with
  | Bot          -> "⊥"
  | Var p        -> p
  | Imp (Imp(_,_) as f, f3) -> "( " ^ string_of_formula f ^ " ) → " ^ string_of_formula f3
  | Imp(f1,f2) ->  string_of_formula f1 ^ " → " ^ string_of_formula f2

let pp_print_formula fmtr f =
  Format.pp_print_string fmtr (string_of_formula f)

type theorem = {
  assumpptions : formula list;
  consequence : formula
}

(*twierdzenie nie musi być drzewem, wystarcza zalozenia i wniosek*)
  
let assumptions (thm : theorem) : formula list = thm.assumpptions

let consequence (thm : theorem) : formula = thm.consequence

let rec equal f1 f2 = 
  match f1, f2 with
  | Bot , Bot -> true
  | Var x, Var y -> x = y
  | Imp(a,b) , Imp(c,d) -> (equal a c) && (equal b d)
  | _ -> false

let pp_print_theorem fmtr thm =
  let open Format in
  pp_open_hvbox fmtr 2;
  begin match assumptions thm with
  | [] -> ()
  | f :: fs ->
    pp_print_formula fmtr f;
    fs |> List.iter (fun f ->
      pp_print_string fmtr ",";
      pp_print_space fmtr ();
      pp_print_formula fmtr f);
    pp_print_space fmtr ()
  end;
  pp_open_hbox fmtr ();
  pp_print_string fmtr "⊢";
  pp_print_space fmtr ();
  pp_print_formula fmtr (consequence thm);
  pp_close_box fmtr ();
  pp_close_box fmtr ()

let by_assumption f = {assumpptions = [f]; consequence = f}

let imp_i f thm = {assumpptions = List.filter (fun g -> g <> f) thm.assumpptions;
                  consequence = Imp(f, thm.consequence)}
let imp_e th1 th2 = 
    match th1.consequence with
    | Imp(f1,f2) -> if equal f1 (th2.consequence) then 
      {assumpptions = List.fold_left (fun acc g -> if List.mem g acc then acc else g :: acc) th1.assumpptions th2.assumpptions;
      consequence = th2.consequence}
      else failwith "wrong reasoning"
    | _          -> failwith "wrong reasoning"

let bot_e f thm = match thm.consequence with
  |  Bot -> {assumpptions = thm.assumpptions; consequence = f}
  | _    -> failwith "wrong reasoning"



(*
#install_printer pp_print_formula;;
#install_printer pp_print_theorem;;

theorems:*)
let p = Var("p")
let q = Var("q")
let r = Var("r")

let th1 = imp_i p (by_assumption p)
let th2 = imp_i p (imp_i q (by_assumption p))

let th4 = imp_i Bot (bot_e p (by_assumption Bot))



let h1 = imp_e  (by_assumption (Imp(p,q))) (by_assumption p)
let h2 = imp_e (by_assumption (Imp(p, Imp(q,r))))  (by_assumption p) 
let h3 = imp_e h2 h1
let h4 = imp_i p h3
let h5 = imp_i (Imp(p,q)) h4
let h6 = imp_i (Imp(p, Imp(q,r))) h5

let th3 = h6