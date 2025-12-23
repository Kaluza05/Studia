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


module FormulaOrd = struct
  type t = formula
  let rec compare a b = 
    match a,b with
    | Bot, Bot -> 0
    | Var x, Var y -> String.compare x y
    | Var _, _ -> 1
    | _ , Var _ -> -1
    | Imp(p1,p2), Imp(p3,p4) -> let p = compare p1 p3 in if p = 0 then compare p2 p4 else p
    | Imp _ , _ -> 1
    | _ , Imp _ -> -1
end

module FSet = Set.Make(FormulaOrd)

type judgement = FSet.t * formula

type theorem = 
  | Node of judgement
  | Timpi of judgement * theorem
  | Timpe of judgement * theorem * theorem
  | Tfalse of judgement * theorem

(*twierdzenie nie musi być drzewem, wystarcza zalozenia i wniosek*)
let real_assumptions (thm : theorem) : FSet.t = 
  match thm with
  | Node(f) | Timpi(f,_) | Timpe(f,_,_) | Tfalse(f,_) -> fst f
  
let assumptions (thm : theorem) : formula list =
  match thm with
  | Node(f) | Timpi(f,_) | Timpe(f,_,_) | Tfalse(f,_) -> FSet.to_list (fst f)


let consequence (thm : theorem) : formula = 
  match thm with
  | Node(f) | Timpi(f,_) | Timpe(f,_,_) | Tfalse(f,_) -> snd f

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

let by_assumption f = Node( FSet.singleton f, f)

let imp_i f thm = Timpi(((FSet.remove f (real_assumptions thm)), Imp(f, consequence thm)) , thm )
let imp_e th1 th2 =
  match consequence th1, consequence th2 with
  | Imp(f1,f2), f3 -> if equal f1 f3 
    then Timpe((FSet.union (real_assumptions th1) (real_assumptions th2),f2), th1, th2) 
    else failwith "wrong reasoning"
  | _ -> failwith "wrong reasoning"

let bot_e f thm =
  match consequence thm with
  | Bot -> Tfalse((real_assumptions thm, f), thm)
  | _ -> failwith "wrong reasoning"




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