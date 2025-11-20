type _ expr =
  | Const : 'a -> 'a expr
  | App   : ('a -> 'b) expr * 'a expr -> 'b expr

let rec eval : type a. a expr -> a =
  function
  | Const x     -> x
  | App(e1, e2) -> (eval e1) (eval e2)

type _ typ =
  | Int : int typ
  | Arr : 'a typ * 'b typ -> ('a -> 'b) typ

type tr_result =
  | Ok : 'a expr * 'a typ -> tr_result

type (_, _) eq =
  | Refl : ('a, 'a) eq

let rec type_eq : type a b. a typ -> b typ -> (a, b) eq option =
  fun tp1 tp2 ->
  match tp1, tp2 with
  | Int, Int -> Some Refl
  | Int, _   -> None
  | Arr(ta1, tv1), Arr(ta2, tv2) ->
    begin match type_eq ta1 ta2, type_eq tv1 tv2 with
    | Some Refl, Some Refl -> Some Refl
    | _ -> None
    end
  | Arr _, _ -> None

let rec tr : UExpr.t -> tr_result =
  function
  | UInt   n   -> Ok(Const n, Int)
  | UConst "+" -> Ok(Const (+), Arr(Int, Arr(Int, Int)))
  | UConst _   -> failwith "unknown constant"
  | UApp(e1, e2) ->
    begin match tr e1 with
    | Ok(e1, Arr(tp2, tp1)) ->
      let (Ok(e2, tp2')) = tr e2 in
      begin match type_eq tp2 tp2' with
      | None -> failwith "type error"
      | Some Refl -> Ok(App(e1, e2), tp1)
      end
    | _ -> failwith "type error"
    end

let eval_int str : int =
  match tr (UExpr.parse str) with
  | Ok(e, Int) -> eval e
  | _ -> failwith "type error"
