open Logic

(* zrob logic.mli, zeby nie udostepniać wszystkiego*)
type bot_game = game * int * color   (*holds bot depth and color*)

let bot_color (_,_,c: bot_game) : color = c

let eval_position (g : game) : float = 
  (* evaluates position, number is > 0 is white is doing better < 0 if black *)
  (match g with
  | Win _ -> -1000
  | Draw -> 0
  | Playing {board;_} -> 
  let curr_player = g  |> get_turn in
  let _turn_sgn = match curr_player with White -> 1 | Black -> -1 in
  Fun.const 0 board
  )
  |> float_of_int


(** returns best move's value *)
let rec alphabeta (g : game) (depth: int) (alpha : float) (beta : float) (maximizing : bool) : float =
  (* maximizing = true if curr color = White *)
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
  (* looks at all possible moves, for each move gets best oponents move and choses the one 
with biggest value for us *)
(* i zaczynamy od maksymalizacji naszego wyniku, wiec minimalizacji przeciwnika? *)
(* patrzymy na nasz kolor *)
  let is_white = g |> get_turn = White in
  let moves = get_all_moves g in 
  let better (score1, _) (score2, _) =
    if is_white then score1 > score2 else score1 < score2
  in
  moves
  |> List.map (fun (curr,go) -> 
    let after_move = g |> move_int curr go in
    let value = alphabeta after_move (depth - 1) Float.neg_infinity Float.infinity false
    in (value, (curr,go)) 
    )
  |> List.fold_left (fun best curr -> if better curr best then curr else best)
     ((if is_white then Float.neg_infinity else Float.infinity), (0,0))
  |> snd
  


let move_bot (g,depth,c : bot_game) : bot_game = 
  match g with
  | Win _ -> failwith "Player won"
  | Draw -> failwith "game ended in a draw"
  | g -> 
    let curr_pos, go_to = find_best_move g depth in
    (g |> move_int curr_pos go_to),depth,c

let move_player (curr_pos : string) (go_to : string) (g,depth,c : bot_game) : bot_game = 
  match g with
  | Win _ -> failwith "bot won"
  | Draw -> failwith "game ended with a draw"
  | Playing _ ->
    let move_p = g |> move curr_pos go_to 
    in  (move_p,depth,c) |> move_bot

(** starts a game with a bot of a given color, if bot is White move bot *)
let game_with_bot (bot_col : color) (depth : int) : bot_game = 
  match bot_col with
  | White -> (init_game (),depth,bot_col) |> move_bot
  | Black -> init_game (),depth,bot_col

let bot_game_to_game (g,_,_ : bot_game) : game = g

let print_bot_game (g,_,_ : bot_game) : unit = print_game g








let nodes_visited = ref 0
let depth_counter = Hashtbl.create 32

let record_depth d =
  let prev = match Hashtbl.find_opt depth_counter d with
    | Some v -> v
    | None -> 0
  in
  Hashtbl.replace depth_counter d (prev + 1)

let reset_stats () =
  nodes_visited := 0;
  Hashtbl.clear depth_counter


let rec minimax (g : game) (depth : int) (maximizing : bool) =

  if depth = 0 || is_terminal (game_to_board g) then ()
    

  else
    let moves = get_all_moves g in
      List.iter (fun (curr,go) ->
        incr nodes_visited;
        record_depth depth;
        let after = move_int curr go g in
        minimax after (depth - 1) (not maximizing)
      )  moves

let find_best_move_minimax g depth =
  reset_stats ();

  let is_white = get_turn g = White in
  minimax g depth is_white