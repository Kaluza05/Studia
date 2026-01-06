open Logic
open Network
open Chessbot

type player_id =  int
type session_id = int

type turn = P1 | P2

type state = 
  | InQueue
  | Idle
  | Playing of turn

type player_info = 
    {player_id  : player_id;
    sock : sock;
    mutable state : state}

type command =
  | Join
  | Play_bot of int * color
  | Leave
  | Help
  | Quit
  | Show_queue
  | All_moves
  | Highlight_move of string
  | Move of move
  | Resign

type parse_error =
  | Unknown_command of string
  | Wrong_arity of string
  | Wrong_arg of string

type parsed =
  | Parsed of command
  | Parse_error of parse_error



type bot_session_info = {session_id : session_id; bot_game : bot_game ; turn : turn} 
type session_info = {session_id : session_id; game : game ; turn : turn}    
(*turn = 1 if it's player one's turn, turn = 0 if it's player two's turn*)
(*maybe it's unnecessary if we but player in a continuation so he won't be able to do anyting*)
type bot_sesssion    = {player : player_info; b_session : bot_session_info}
type players_session = {player_1 : player_info; player_2 : player_info; p_session : session_info}

type session = 
  | Player of players_session
  | Bot    of bot_sesssion

let update_turn (t : turn) : turn = 
  match t with
  | P1 -> P2
  | P2 -> P1

let turn_of_state (s : state) : turn = 
  match s with
  | Playing t -> t
  | _ -> failwith "wrong state"

let parsed_to_command (p : parsed) : command =
  match p with
  | Parsed c -> c
  | _ -> failwith "parse error" 

let parse_line (s : string) : parsed =
  let toks =
    s |> String.trim |> String.lowercase_ascii |> String.split_on_char ' '
    |> List.filter ((<>) "")
  in
  print_list_list Fun.id [toks];
  match toks with
  | [] -> Parse_error (Unknown_command "")
  | "join" :: _ -> Parsed Join
  | "playbot" :: [i;c] -> Parsed (Play_bot( (int_of_string i), string_to_color c))
  | "playbot" :: _ -> Parse_error (Wrong_arity "play bot")
  | "leave" :: _ -> Parsed Leave
  | "highlight" :: [sq] -> Parsed (Highlight_move sq)
  | "highlight" :: _ -> Parse_error (Wrong_arity "highlight")
  | "move" :: [curr; go] -> Parsed (Move (curr,go))
  | "move" :: _ -> Parse_error (Wrong_arity "move")
  | "resign" :: _ -> Parsed Resign
  | "help" :: _ -> Parsed Help
  | "quit" :: _ -> Parsed Quit
  | "show" :: "queue" :: _ -> Parsed Show_queue
  | cmd :: _ -> Parse_error (Unknown_command cmd)


let pid = ref 0
let sess_id = ref 0 
let give_id () = 
  pid := !pid + 1;
  !pid

let give_sess_id () = 
  sess_id := !sess_id + 1;
  !sess_id

let queue_to_play = Queue.create ()

let sessions : (session_id, session) Hashtbl.t = Hashtbl.create 10
let string_of_list (to_str : 'a -> string) (xs : 'a list) : string = 
  let rec it xs = 
    match xs with
    | [] -> "]"
    | x :: [] ->  to_str x ^ "]"
    | x :: y :: xs -> to_str x ^ "; " ^ it (y :: xs)
  in "[" ^ it xs
  
let command_to_string (c : command) : string = 
  match c with
  | Join           -> "Join"
  | Play_bot(d,c)  -> "Play bot: depth : " ^ string_of_int d ^ ", color: " ^ string_of_color c
  | Leave          -> "Leave"
  | Highlight_move(p) -> "Highlight move: " ^ p 
  | Move(p1,p2)    -> "Move(" ^ p1 ^ ", " ^ p2 ^ ")"
  | All_moves      -> "All moves"
  | Resign         -> "Resign"
  | Help           -> "Help"
  | Quit           -> "Quit"
  | Show_queue     -> "Show Queue"

let string_of_queue (to_str : 'a -> string) (q : 'a Queue.t) = q |> Queue.to_seq |> List.of_seq |> string_of_list to_str


let create_bot_session (p : player_info) (i,c) : session = 
  let new_game = {session_id = give_sess_id (); bot_game = game_with_bot c i; turn = match c with | White -> P2 | Black -> P1} in
  (* if bot color is white then he will already make a move *)
  let new_session =  Bot {player = p; b_session = new_game} in
  Hashtbl.add sessions new_game.session_id new_session;
  new_session

let create_session (p1 : player_info) (p2 : player_info) : session = 
  let new_game = {session_id = give_sess_id (); game = init_game () ; turn = P1} in
  let new_session = Player {player_1 = p1; player_2 = p2; p_session = new_game} in
  Hashtbl.add sessions new_game.session_id new_session;
  new_session


let play_bot_game (game : bot_sesssion) cont =
  let open Chessbot in
  let p, game = game.player, game.b_session.bot_game in
  let rec loop game = 
    output_string p.sock "select action (highlight,move,resign): " (fun () -> 
    input_line p.sock (fun line_opt ->
      match line_opt with
      | None -> exit ()
      | Some s -> 
        begin match parse_line s with
        | Parsed Resign -> 
          output_string p.sock ("Player " ^ (game |> bot_game_to_game |> resign |> get_winner |> color_to_string) ^ " won, by resignation\n")
          (fun () -> cont ())
        | Parsed (Highlight_move pos) ->
          output_string p.sock (highlight_move_string pos (game |> bot_game_to_game)) (fun () -> loop game)
        | Parsed (Move (p1,p2)) ->
          let after_move = move_player p1 p2 game in
          loop after_move
          (* moze nie loopowac sie od razu tylko przekazać to do kontynuacji? *)
        | Parsed c -> failwith ("parsed but wrong" ^ command_to_string c)
        | Parse_error _ -> failwith "parse error"
        end)
        
          )
  in loop game
  (*get commands: possible ones will be Highlight_move, move, resign, quit*)

let rec choose_game_action p (g : game) (cont : command -> ans) =  
    output_string p.sock "" (fun () ->
    input_line p.sock (fun line_opt ->
      match line_opt with
      | None -> cont Resign
      | Some s ->
        match parse_line s with
        | Parsed Resign -> cont Resign
        | Parsed All_moves ->
          output_string p.sock ("Possible moves from pos\n" ^ highlight_all_moves_string g) 
          (fun () -> choose_game_action p g cont) 
        | Parsed (Highlight_move pos) -> 
          output_string p.sock ("Possible moves from pos\n" ^ highlight_move_string pos g) 
          (fun () -> choose_game_action p g cont) 
        | Parsed Help ->
          output_string p.sock "Commands: move | resign | highlight\n"
          (fun () -> choose_game_action p g cont)
        | Parsed (Move(curr,go)) -> cont (Move(curr,go))
        | _ -> choose_game_action p g cont
              ))

let rec play_game game cont = 
  (* switching between continuations asking for a move of P1 then continuation with move for P2 then repeat *)
  (* or asking for P1 move then continuation for play_game with move P2 *)
  let p1,p2,gm,turn = game.player_1, game.player_2, game.p_session.game, game.p_session.turn in
  match gm with
  | Draw -> 
    output_string p1.sock "Game ended with a draw.\n" (fun () ->
    output_string p2.sock "Game ended with a draw.\n" (fun () -> cont ()))
  | Win c -> 
    (* for now p1 always plays white  *)
    let who_won c' = 
      if c = c' then "won" else "lost"
    in

    output_string p1.sock ("Game ended, you "^ who_won White ^ ".\n") (fun () ->
    output_string p2.sock ("Game ended, you "^ who_won Black ^ ".\n") (fun () -> cont ()))
  | Playing _ -> 
    (* chcemy wziac  *)
    if turn = turn_of_state p1.state then  
    output_string p1.sock ("Your move, current board\n" ^ show_board gm ^ "What do you wanna do:") (fun () ->
      choose_game_action p1 gm (fun action -> 
      let new_game = 
        match action with
        | Resign -> if turn = P1 then Win Black else Win White
        | Move (curr,go) -> move curr go gm
        | _ -> failwith "impossible case"
      in
      let new_state = {player_1=p1;player_2=p2;
                      p_session = {session_id = game.p_session.session_id;game = new_game;turn = update_turn turn}} 
    in play_game new_state cont))
    else 
      output_string p2.sock ("Your move, current board\n" ^ show_board gm ^ "What do you wanna do:") (fun () ->
      choose_game_action p2 gm (fun action ->
      let new_game = 
        match action with
        | Resign -> if turn = P1 then Win Black else Win White
        | Move (curr,go) -> move curr go gm
        | _ -> failwith "impossible case"
      in
      let new_state = {player_1=p1;player_2=p2;
                      p_session = {session_id = game.p_session.session_id;game = new_game;turn = update_turn turn}} 
    in play_game new_state cont))

      (* wziac ruch do wykonania, zaaktualizowac wszystko potrzebne, uruchomic kontynuacje *)


let start_game (game : session) cont = 
  match game with
  | Bot game -> 
    let p,c = game.player, !!(bot_color game.b_session.bot_game) in
    p.state <- Playing (match c with | White -> P1 | Black -> P2);
    output_string p.sock ("Bot game started. You play "^ color_to_string c ^ ".\n") (fun () ->
      play_bot_game game (fun () -> cont ()))
  | Player game ->
    let p1, p2 = game.player_1, game.player_2 in
    p1.state <- Playing P1;
    p2.state <- Playing P2;

    output_string p1.sock "Game found. You play white.\n" (fun () ->
    output_string p2.sock "Game found. You play black.\n" (fun () ->
      play_game game (fun () -> cont ())
        (* po zakończeniu wracamy do menu *)
        ))

let try_start_game cont_success cont_fail = 
  if Queue.length queue_to_play >= 2 then 
    begin
    let p1 = Queue.pop queue_to_play in
    let p2 = Queue.pop queue_to_play in

    let session = create_session p1 p2 in

    start_game session (fun () -> cont_success p1 p2 ())
    end 
    else
    cont_fail ()

let try_bot_game p (i,c) cont = 
  match p.state with
  | InQueue | Playing _ -> cont ()
  | Idle -> 
    let session = create_bot_session p (i,c) in
      start_game session (fun () -> 
        p.state <- Idle;
        cont ())


let join_queue (player : player_info) cont_success cont_fail = 
  match player.state with
  | InQueue ->
      output_string player.sock "You are already in queue\n" cont_fail
  
  | Playing _->
      output_string player.sock "You are already in a game\n" cont_fail
  
  | Idle ->
      Queue.push player queue_to_play;
      player.state <- InQueue;
      output_string player.sock "Joined queue. Waiting for opponent...\n" (fun () ->
        try_start_game cont_success cont_fail)

let leave_queue (player : player_info) cont = 
  match player.state with
  | InQueue ->
      let tmp = Queue.fold (fun acc p -> if p.player_id = player.player_id then acc else p :: acc) [] queue_to_play in
      Queue.clear queue_to_play;
      List.iter (fun p -> Queue.push p queue_to_play) (List.rev tmp);
      player.state <- Idle;
      output_string player.sock "Left queue\n" cont

  | _ ->
      output_string player.sock "You are not in queue\n" cont

let show_queue (player : player_info) cont = 
    output_string player.sock "" cont

let rec loop_for_player ?(first = false) (p : player_info) () = 
    let mess = if first then
    "Here's your id: " ^ string_of_int p.player_id ^ 
    "\nHere's current game queue : " ^ string_of_queue (fun q -> string_of_int q.player_id) queue_to_play ^ 
    "\nInput What you want to do, if you don't know type \"help\": "
    else 
    "Current game queue : " ^ string_of_queue (fun q -> string_of_int q.player_id) queue_to_play ^ 
    "\nInput What you want to do, if you don't know type \"help\": "
    in
    let rec get_command mess cont = 
    match p.state with
    | InQueue | Playing _ -> exit ()
    | Idle ->
    output_string p.sock mess (fun () ->
    input_line p.sock (fun line_opt ->
      match line_opt with
      | None -> 
        exit ()
      | Some s ->
    begin match parse_line s with
    | Parsed Help -> 
      let mess = "
      List of possible commands:\n
      join - joins current FILO queue\n
      leave - leaves curent queue, if you are currently in a queue\n
      playbot d c - starts the game with a bot of a difficulty d : int, and color c (avalible colors: white, black)\n
      show - after every turn shows current board, (it shouldnt be a command but for now it is)\n
      highlight p - during players turn shows all possible places to go from posision p (ex. highlight E2)\n
      move p1 p2 - during players turn moves figure from p1 to p2\n  
      resign - Aborts the game if player was in it\n
      help - Lists every command avalible\n
      quit - Shuts down program for yourself\n
      \n
      What do you want to do? : " in
      get_command mess cont
    | Parsed (Play_bot (i,c)) ->
      try_bot_game p (i,c) (fun () -> cont ())
    | Parsed Join -> 
      let cont_success p1 p2 () =  
        p1.state <- Idle;
        p2.state <- Idle;
        loop_for_player p1 () |> ignore;
        loop_for_player p2 ()
      in
      join_queue p cont_success (fun () -> cont ())
    | Parsed Quit -> 
      output_string p.sock "Quitting program, Bye" (fun () -> 
      close p.sock;
      exit ())
    | Parsed Leave  -> 
      leave_queue p (fun () -> cont ())
    | Parsed Show_queue -> show_queue p (fun () -> cont ())
    | Parse_error (Unknown_command s) ->
      let error_mess = "Unknown command: " ^ s ^ "\ntry again: " in
      get_command error_mess cont;
    | Parse_error (Wrong_arity s) ->
      let error_mess = "Wrong arity: " ^ s ^ "\ntry again: " in
      get_command error_mess cont;
    | Parse_error (Wrong_arg s) ->
      let error_mess = "Wrong arg: " ^ s ^ "\ntry again: " in
      get_command error_mess cont;
    | _ -> 
      let error_mess = "something went wrong with your command or wrong command, try again\n" in
      get_command error_mess cont;
    end))
    in
    
    get_command mess (fun () -> loop_for_player p ())



let proc_client sock =
  let player = {player_id = give_id (); sock = sock;state = Idle} in
  loop_for_player ~first:true player ()
    

let () = establish_server ~port:1234 proc_client



(*aktualnie mimo ze jest się w kolejce mozna pisać do menu i jest jakis problem ze start game wtedy,
chwilowy fix moze byc taki zeby nie podawac zadnej kontynuacji jak się wchodzi do kolejki,
albo kontynuacje oczekującą czy dołączyło się do gry*)