module type Game = sig
  type position = Logic.position
  type board
  type repetition_tracker
  type no_takes_tracker
  type coding
  type color
  type move

  type game_representation = 
    {
    board : board;
    reperirtion_tracker : repetition_tracker;
    no_takes_tracker : no_takes_tracker;
    coding : coding
    }

  type game = 
  | Playing of game_representation
  | Win of color
  | Draw

  val init_game : unit -> game
  val move : move -> game -> game
  val move_opt : move -> game -> game option

  val visible : color -> game -> position

  val game_to_position : game -> position
end


module GameState(G : Game) : sig 
  type history
  type game_state = 
    {
    current : G.game;
    history : history
    }
    
  val return : G.game -> game_state
  val bind :  game_state -> (G.game -> game_state) -> game_state

  val move :  G.move -> game_state -> game_state
  val undo :  game_state -> game_state option

  val get  : game_state -> G.game
  val get_history : game_state -> history
end = struct
  type history = G.game list
  type game_state = {
  current : G.game;
  history : G.game list;
  }

  let return g = {current = g; history = []}
  let bind gst f = 
    let {current = new_curr; history = _} = f gst.current in
    {current = new_curr; history = gst.current :: gst.history}

    (* {current = f c ; history = c :: hist} *)
  let (let*) = bind
  let move mv gst =
    let* g = gst in 
    g |> G.move mv |> return

  let undo gst = 
    match gst.history with
    | [] -> None
    | x :: xs -> Some {current = x; history = xs}

  let get gst = gst.current
  let get_history gst = gst.history
end
