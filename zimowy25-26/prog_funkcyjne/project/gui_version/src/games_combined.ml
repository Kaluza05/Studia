type game_type = 
  | Regular of Logic.game
  | Chess960 of Chess960.game
  | FogOfWar of Fogofwar.game

let game_type_by_game (g : game_type) = 
match g with
  | Regular _  -> "REGULAR"
  | Chess960 _ -> "CHESS960"
  | FogOfWar _ -> "FOG"

let starting_board game_type = 
  match game_type with
  | "REGULAR"  -> Regular (Logic.init_game ())
  | "CHESS960" -> Chess960 (Chess960.init_game ())
  | "FOG"      -> FogOfWar (Fogofwar.init_game ())
  | _ -> failwith "wrong queue type"

let make_move (g : game_type) from go = 
  match g with
  | Regular  g -> 
    begin match g |> Logic.move_opt from go with
    | None -> None
    | Some g -> Some (Regular g)
    end
  | Chess960 g ->
    begin match g |> Chess960.move_opt (from,go) with
    | None -> None
    | Some g -> Some (Chess960 g)
    end
  | FogOfWar g -> 
    begin match g |> Fogofwar.move_opt (from,go) with
    | None -> None
    | Some g -> Some (FogOfWar g)
    end

let game_to_position (g : game_type) = 
  match g with
  | Regular  g -> g |> Logic.game_to_position 
  | Chess960 g -> g |> Chess960.game_to_position
  | FogOfWar g -> g |> Fogofwar.game_to_position 

let game_to_visible (c : Logic.color) (g : game_type) = 
  match g with
  | Regular  _ -> g |> game_to_position   |> Logic.game_to_fen
  | Chess960 _ -> g |> game_to_position   |> Logic.game_to_fen
  | FogOfWar g -> g |> Fogofwar.visible c |> Logic.game_to_fen 

let is_draw (g : game_type) : bool = 
  match g with
  | Regular Draw  -> true
  | Chess960 Draw -> true
  | FogOfWar Draw -> true
  | Regular _     -> false
  | Chess960 _    -> false
  | FogOfWar _    -> false

let is_win (g : game_type) : bool = 
  match g with
  | Regular (Win _)   -> true
  | Chess960 (Win _ ) -> true
  | FogOfWar (Win _)  -> true
  | Regular _         -> false
  | Chess960 _        -> false
  | FogOfWar _        -> false

let get_winner (g : game_type) : Logic.color = 
  match g with
  | Regular  (Win c) -> c
  | Chess960 (Win c) -> c
  | FogOfWar (Win c) -> c
  | _ -> failwith "not implemented get_winner"

let get_turn (g : game_type) : Logic.color = 
  match g with
  | Regular g -> g |> Logic.get_turn
  | Chess960 g -> g |> Chess960.get_turn
  | FogOfWar g -> g |> Fogofwar.get_turn

let highlight_squares (pos : string) (g : game_type) = 
  match g with
  | Regular g  -> g |> Logic.highlight_squares pos
  | Chess960 g -> g |> Chess960.highlight_squares pos
  | FogOfWar g -> g |> Fogofwar.highlight_squares pos