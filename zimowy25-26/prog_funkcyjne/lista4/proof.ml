open Logic

type proof_node = 
  | Goal      of (string * formula) list * formula
  | Theorem   of theorem                           
  | Imp_i     of (string * formula) * proof_node 
  | Imp_e     of proof_node * proof_node       
  | Bot_e     of proof_node * formula            

type ctx = 
  | CTop
  | CImp    of ctx * (string * formula)   (* for both intro and elimination of imp to know how to rebuild*)
  | CLefte  of ctx * proof_node 
  | CRighte of ctx * proof_node 
  | CBot    of ctx * formula              (* żeby trzymac jaka formule chcemy wyprowadzic z fałszu*)

type proof = 
  | Complete of theorem
  | Incomplete of ctx * (string * formula) list * formula   (* trzymamy kontekts + to co trzymamy to jest goal wiec dajemy typ jaki ma Goal*)



let proof assm f = Incomplete(CTop,assm,f)

let qed pf =
  match pf with
  | Complete(thm) -> thm 
  | Incomplete _  -> failwith "proof not complete"

(**returns info about current goal*)
let goal pf =
  match pf with
  | Complete thm           -> None
  | Incomplete(ctx,assm,f) -> Some(assm,f)


(*looks where there is next goal*)

let rec next_goal node ctx = 
  (*simplifies theorems when going up when possible*)
  let simpl node = 
    match node with
    | Theorem thm     -> Theorem thm
    | Goal(assm,f)    -> Goal(assm,f)
    | Imp_i((_,f),n)  -> 
        begin match n with
        | Theorem(th) -> Theorem(imp_i f th)
        | _ -> node
        end

    | Imp_e(n1,n2) -> 
      begin match n1,n2 with
        | Theorem(th1),Theorem(th2) -> Theorem(imp_e th1 th2)
        | _ -> node
      end

    | Bot_e(n,f) ->
      begin match n with
      | Theorem(th) -> Theorem(bot_e f th)
      | _ -> node
      end
    in

  (* going up the context and going into different branches*)
  (* when we have elimination of implication we first go left, and then when we didn't find a goal jump to right and there go left instead of going up*)
  let rec go_up node ctx = 
    match ctx with
    | CTop ->  
      begin match node with 
      | Theorem thm -> Complete thm
      | _           -> go_down node ctx
      end
    | CImp(ctx',(name,f))  -> go_up (simpl (Imp_i((name,f),node))) ctx' 
    | CLefte(ctx', node2)  -> go_down node2 (CRighte(ctx',node)) 
    | CRighte(ctx', node3) -> go_up (simpl (Imp_e(node3,node))) ctx' 
    | CBot(ctx',f)         -> go_up (simpl (Bot_e(node,f))) ctx' 

(* going down some path changing context as we go until we find next goal*)
  and go_down node ctx = 
    match node with
    | Theorem(th)           -> go_up node ctx                     (* cant go further *)
    | Goal(assm,f)          -> Incomplete(ctx,assm,f)
    | Imp_i((name,f),node') -> go_down node' (CImp(ctx,(name,f))) (*going further down*)
    | Imp_e(node1,node2)    -> go_down node1 (CLefte(ctx,node2))  (*going down only left path, we will visit right path when going back up*)
    | Bot_e(node',f )       -> go_down node' (CBot(ctx,f))        (*going further down *)

  in match node with
  | Goal(assm,f) -> go_up node ctx      (*that means we start from goal, and we want to find next goal so we go up*)
  | _            -> go_down node ctx    (*case where we arent in a goal, so first we go down, then*)

(* next_goal is going to look for the next goal in the proof by "traversing context"*)
(* changes current goal to prove along with context*)
let next pf = 
  match pf with
  | Complete _             -> failwith "proof complete"
  | Incomplete(ctx,assm,f) -> next_goal (Goal(assm,f)) ctx  (*looking for a goal starting in current goal*)

(* if f is imp f1 -> f2 we add f1 to assumptions, and go down in context to CImp(ctx,assumption, imp)*)
let intro (name : string) (pf : proof) : proof =
  match pf with
  | Complete thm            -> failwith "proof is complete"
  | Incomplete (ctx,assm,f) -> 
    match f with
    | Imp(f1,f2) -> let new_assm = (name, f1) :: assm in 
      Incomplete(CImp(ctx, (name,f1)),new_assm,f2)
    | _ -> failwith "trying to intro not on implication"
  
let apply f pf = 
  match pf with
  | Complete thm                    -> failwith "already proven"
  | Incomplete(ctx, assm, to_prove) -> 
    let rec make_chain decomposed_f acc = (*tworzy to całe drzewo*)
        match decomposed_f with
        | _ when decomposed_f = to_prove -> (acc,decomposed_f)
        | Imp(f1, f2)                    -> make_chain f2 (f1 :: acc)
        | _                              -> (acc, decomposed_f)
      in
      let decomp, last = make_chain f [] in (*decomp lista ktora zamienila f0 -> f1 -> ... -> fn w [fn,fn-1,..,f0], last to f albo Bot*)
      let is_bot = 
        (match last with
        | fi when fi = to_prove -> false
        | Bot -> true
        | _ -> failwith "formula doesnt imply bot or to_prove")
      in 
      let ctx = if is_bot then CBot (ctx, to_prove) else ctx in (*if f is proving Bot then we extend context*)
      let new_ctx = 
        List.fold_left (fun acc fi -> CLefte(acc,Goal(assm,fi))) ctx decomp  (* builds context down from the current context layer by layer*)
      in
      Incomplete (new_ctx, assm, f)


let apply_thm thm pf =
  (* sprawdzic ze zał thm są podzbiorem dostępnych zalozen*)
  let f = consequence thm in
  match apply f pf with
  | Complete thm        -> failwith "proof complete"
  | Incomplete(ctx,_,_) -> next_goal (Theorem(thm)) ctx     (*udowadniamy od razu f0 i szukamy nastepnego celu*)


(* z załozenia f tworzymy twierdzenie {f} |- f  i z niego korzystamy *)
let apply_assm name pf = 
  match pf with
  | Complete thm -> failwith "proof complete"
  | Incomplete(ctx,assm,f) -> 
    let f = List.assoc name assm in
    let thm = by_assumption f in
    apply_thm thm pf


let pp_print_proof fmtr pf =
  match goal pf with
  | None -> Format.pp_print_string fmtr "No more subgoals"
  | Some(g, f) ->
    Format.pp_open_vbox fmtr (-100);
    g |> List.iter (fun (name, f) ->
      Format.pp_print_cut fmtr ();
      Format.pp_open_hbox fmtr ();
      Format.pp_print_string fmtr name;
      Format.pp_print_string fmtr ":";
      Format.pp_print_space fmtr ();
      pp_print_formula fmtr f;
      Format.pp_close_box fmtr ());
    Format.pp_print_cut fmtr ();
    Format.pp_print_string fmtr (String.make 40 '=');
    Format.pp_print_cut fmtr ();
    pp_print_formula fmtr f;
    Format.pp_close_box fmtr ()


let p,q,r = Var("p"),Var("q"), Var("r")
let h11 = Imp(p, Imp(q,r))
let h12 = Imp(p,q)
let h21 = Imp(p,Bot)
let h22 = Imp(Imp(h21,p),p)
let h31 = Imp(Imp(h21,Bot),p)
let th1 = Imp(h11,Imp(h12,Imp(p,r)))
let th2 = Imp(h22,h31)
let th3 = Imp(h31,h22)

let proof1 = proof [] th1 |> intro "H1" |> intro "H2" |> intro "H3" |> apply_assm "H1" |> apply_assm "H3" |> apply_assm "H2" |> apply_assm "H3" |> qed
let proof2 = proof [] th2 |> intro "H1" |> intro "H2" |> apply_assm "H1" |> intro "H3" |> apply_assm "H2" |> apply_assm "H3" |> qed
let proof3 = proof [] th3 |> intro "H1" |> intro "H2" |> apply_assm "H1" |> intro "H3" |> apply_assm "H3" |> apply_assm "H2" |> apply_assm "H3" |> qed