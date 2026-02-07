open Logic
type position = Logic.position
type color = Logic.color
type board = position * color * Logic.castle_rights * ((Logic.color * int) option)
type move = string * string
type repetition_tracker = (int,int) Hashtbl.t
type no_takes_tracker = int
type coding = string list

type game_representation = 
  {
  board : board;
  repetition_tracker : repetition_tracker;
  no_takes_tracker : no_takes_tracker;
  coding : coding
  }

type game = 
| Playing of game_representation
| Win of color
| Draw


let game_to_position (g : game) : position = 
  match g with
  | Playing {board;_}-> let p,_,_,_ = board in p
  | _ -> failwith "game ender when converting to position fog"

let get_turn (g : game) = 
  match g with
  | Playing {board;_}-> let _,t,_,_ = board in t
    | _ -> failwith "game ender when converting to turn fog"

let game_to_state (g : game) : Logic.game = 
  match g with
  | Playing g -> 
    Playing {board = g.board;repetitions = g.repetition_tracker; no_takes_counter = g.no_takes_tracker; coding = g.coding}
  | Win c -> Win c
  | Draw -> Draw

let state_to_game (g : Logic.game) : game = 
  match g with
  | Playing g -> 
    Playing {board = g.board;repetition_tracker = g.repetitions; no_takes_tracker = g.no_takes_counter; coding = g.coding}
  | Win c -> Win c
  | Draw -> Draw
    
let init_game () = init_game () |> state_to_game

let highlight_squares p (g : game) = highlight_squares p (game_to_state g)

let visible (col : color) (g : game) : position = 
  match g with
  | Playing {board;_} -> 
    let b,c,cast,en = board in 
    let pieces = Logic.all_pieces (b,c,cast,en) col in (*pozycje wszystkich naszych figur, dla kazdej znalezc co atakuje*)
    let attacked = List.map (fun (r,c) -> Logic.flatten_pos r c) (List.concat_map (Logic.attacked_positions (b,c,cast,en)) pieces) in
    let visible = pieces @ attacked in 
    List.mapi (fun i x -> if List.mem i visible then x else None) b
    
  | _ -> failwith "nothing visible game ended"

let move ?(promo = Queen) (from,go : move) (g : game) = 
  g |> game_to_state |> Logic.move from go ~promo:promo |> state_to_game
  
let move_opt ?(promo = Queen) (from, go : move) (g : game) = 
  match g |> game_to_state |> Logic.move_opt from go ~promo:promo with
  | None -> None
  | Some g -> Some (g |> state_to_game)