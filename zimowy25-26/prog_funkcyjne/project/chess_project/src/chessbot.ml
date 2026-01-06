open Logic

(* zrob logic.mli, zeby nie udostepniać wszystkiego*)
type bot_game = game * int * color   (*holds bot depth and color*)

let eval_position (g : game) : float = 
  let curr_player = g  |> get_turn in
  (match g with
  | Win c -> if c = curr_player then 1000 else -1000
  | Draw -> 0
  | Playing {board;_} -> Fun.const 1 board)
  |> float_of_int


(** returns best move's value *)
let rec alphabeta (g : game) (depth: int) (alpha : float) (beta : float) (maximizing : bool) : float =
  let exception Pruning of float 
  in
  if depth = 0 || is_terminal (game_to_board g) then eval_position g
  else 
  let moves = get_all_moves g in
  if maximizing then
    (* returning new alpha *)
    try
      List.fold_left (fun acc (curr,go) -> 
        let after_move = g |> move_int curr go in
        let value = max acc (alphabeta after_move (depth - 1) alpha beta false) in
        if value >= beta then raise (Pruning value) else value ) 
      Float.neg_infinity moves
    with
    | Pruning value -> max alpha value

  else
    (* returning new beta *)
    try 
      List.fold_left (fun acc (curr,go) -> 
        let after_move = g |> move_int curr go in
        let value = min acc (alphabeta after_move (depth - 1) alpha beta true) in
        if value <= alpha then raise (Pruning value) else value)
      Float.infinity moves
    with
    | Pruning value -> min beta value

(** searches for the best move currently up to depth and returns the move *)
let find_best_move (g : game) (depth : int) : int * int = 
  let moves = get_all_moves g in 
  moves
  |> List.fold_left (fun (acc,best) (curr,go) -> 
    let after_move = g |> move_int curr go in
    let value = alphabeta after_move (depth - 1) Float.neg_infinity Float.infinity false in
    if value > acc then (value,(curr,go)) else (acc,best)
    ) (Float.neg_infinity, (0,0))
  |> snd


let move_bot (g,depth,c : bot_game) : bot_game = 
  match g with
  | Win _ | Draw -> failwith "game already finished"
  | g -> 
    let curr_pos, go_to = find_best_move g depth in
    (g |> move_int curr_pos go_to),depth,c

let move_player (curr_pos : string) (go_to : string) (g,depth,c : bot_game) : bot_game = 
  let move_p = g |> move curr_pos go_to 
  in  (move_p,depth,c) |> move_bot

(** starts a game with a bot of a given color, if bot is White move bot *)
let game_with_bot (bot_col : color) (depth : int) : bot_game = 
  match bot_col with
  | White -> (init_game,depth,bot_col) |> move_bot
  | Black -> init_game,depth,bot_col

let bot_game_to_game (g,_,_ : bot_game) : game = g