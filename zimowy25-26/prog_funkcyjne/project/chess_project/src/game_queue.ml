open Logic
open Network

type player_id =  int
type session_id = int

type state = 
  | InQueue
  | Idle
  | Playing

type player_info = 
    {player_id  : player_id;
    sock : sock;
    mutable state : state}

type command =
  | Join
  | Play_bot of int * color
  | Leave
  | Show_board
  | Highlight_move of string
  | Move of move
  | Resign
  | Help
  | Quit

type parse_error =
  | Unknown_command of string
  | Wrong_arity of string
  | Wrong_arg of string

type parsed =
  | Parsed of command
  | Parse_error of parse_error

type turn = P1 | P2
type session_info = {session_id : session_id; game : game; turn : turn}    
(*turn = 1 if it's player one's turn, turn = 0 if it's player two's turn*)
(*maybe it's unnecessary if we but player in a continuation so he won't be able to do anyting*)
type bot_sesssion    = {player : player_info; session : session_info}
type players_session = {player_1 : player_info; player_2 : player_info; session : session_info}

type session = 
  | Player of players_session
  | Bot    of bot_sesssion


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
  | "show" :: _ -> Parsed Show_board
  | "highlight" :: [sq] -> Parsed (Highlight_move sq)
  | "highlight" :: _ -> Parse_error (Wrong_arity "highlight")
  | "move" :: [curr; go] -> Parsed (Move (curr,go))
  | "move" :: _ -> Parse_error (Wrong_arity "move")
  | "resign" :: _ -> Parsed Resign
  | "help" :: _ -> Parsed Help
  | "quit" :: _ -> Parsed Quit
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
  | Show_board     -> "Show board"
  | Highlight_move(p) -> "Highlight move: " ^ p 
  | Move(p1,p2)    -> "Move(" ^ p1 ^ ", " ^ p2 ^ ")"
  | Resign         -> "Resign"
  | Help           -> "Help"
  | Quit           -> "Quit"

let string_of_queue (to_str : 'a -> string) (q : 'a Queue.t) = q |> Queue.to_seq |> List.of_seq |> string_of_list to_str

let handle_bot_game sock (i,c : int * color) cont =
  let open Chessbot in
  let game = game_with_bot c i in 
  let rec loop game = 
    output_string sock "select action (highlight,move,resign): " (fun () -> 
    input_line sock (fun line_opt ->
      match line_opt with
      | None -> exit ()
      | Some s -> 
        begin match parse_line s with
        | Parsed Resign -> 
          output_string sock ("Player " ^ (game |> bot_game_to_game |> resign |> get_winner |> color_to_string) ^ " won, by resignation\n")
          (fun () -> cont ())
        | Parsed (Highlight_move p) ->
          output_string sock (highlight_move_string p (game |> bot_game_to_game)) (fun () -> loop game)
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

let create_bot_session (p : player_info) : session = 
  let new_game = {session_id = give_sess_id (); game = init_game; turn = P1} in
  let new_session =  Bot {player = p; session = new_game} in
  Hashtbl.add sessions new_game.session_id new_session;
  new_session
let create_session (p1 : player_info) (p2 : player_info) : session = 
  let new_game = {session_id = give_sess_id (); game = init_game ; turn = P1} in
  let new_session = Player {player_1 = p1; player_2 = p2; session = new_game} in
  Hashtbl.add sessions new_game.session_id new_session;
  new_session

let play_game p1 p2 game cont = cont ()
(* obsluga calej gry czyli ruchy, zmiany tur,poddanie się itd. *)
 
let start_player_game (p1 : player_info) (p2 : player_info) (game : session) = 
  p1.state <- Playing;
  p2.state <- Playing;

  output_string p1.sock "Game found. You play white.\n" (fun () ->
  output_string p2.sock "Game found. You play black.\n" (fun () ->
    play_game p1 p2 game (fun () ->
      (* po zakończeniu wracamy do menu *)
      p1.state <- Idle;
      p2.state <- Idle;
      loop_for_player p1 ();
      loop_for_player p2 ())))

let try_start_game cont = 
  if Queue.length queue_to_play >= 2 then 
    begin
    let p1 = Queue.pop queue_to_play in
    let p2 = Queue.pop queue_to_play in

    let game = create_session p1 p2 in

    start_player_game p1 p2 game cont
    end 
    else
    cont ()

let end_game p1 p2 cont =
  p1.state <- Idle;
  p2.state <- Idle;
  cont ()


let join_queue (player : player_info) cont = 
  match player.state with
  | InQueue ->
      output_string player.sock "You are already in queue\n" cont
  
  | Playing ->
      output_string player.sock "You are already in a game\n" cont
  
  | Idle ->
      Queue.push player queue_to_play;
      player.state <- InQueue;
      output_string player.sock "Joined queue. Waiting for opponent...\n" (fun () ->
        try_start_game cont)

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

let proc_client sock =
  let player = {player_id = give_id (); sock = sock;state = Idle} in
  let welcome_mess = 
    "Here's your id: " ^ string_of_int player.player_id ^ 
    "\nHere's current game queue : " ^ string_of_queue (fun q -> string_of_int q.player_id) queue_to_play ^ 
    "\nInput What you want to do, if you don't know type \"help\": " in
  let command = ref Quit in 
  let rec start_command mess (cont : unit -> ans) = 
  output_string sock mess (fun () ->
  input_line sock (fun line_opt ->
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
      start_command mess cont
    | Parsed (Play_bot (i,c)) ->
      command := Play_bot (i, c);
      cont ()
    | Parsed Join | Parsed Quit as c -> 
      command := c |> parsed_to_command ;
      cont ()
    | Parse_error (Unknown_command s) ->
      let error_mess = "Unknown command: " ^ s ^ "\ntry again: " in
      start_command error_mess cont;
    | Parse_error (Wrong_arity s) ->
      let error_mess = "Wrong arity: " ^ s ^ "\ntry again: " in
      start_command error_mess cont;
    | Parse_error (Wrong_arg s) ->
      let error_mess = "Wrong arg: " ^ s ^ "\ntry again: " in
      start_command error_mess cont;
    | _ -> 
      let error_mess = "something went wrong with your command or wrong command, try again\n" in
      start_command error_mess cont;
    end))
    
    in
    let rec loop mess = 
    start_command mess (fun () -> 
    match !command with
    | Quit -> 
      output_string sock "Quitting program, Bye" (fun () -> 
      close sock;
      exit ())
    | Play_bot(i,c) ->
       (*handle_bot_game 
       nie jest w kolejce wiec nie trzeba go z tamtad zabierać,
       jakos trzeba pozwolić mu grać z botem*)
       print_endline "inside play bot ";
       handle_bot_game sock (i,c) (fun () -> loop "What do you want to do now, play another game?: ");
    | Join ->
      join_queue player (fun () -> loop mess)
      (*handle player in queue, moze wyjsc wiec cofnac sie do poprzedniego wyboru tak na prawde, 
      trzeba tam dodac sprawdzenie czy nie jest w kolejce*)
      (* Queue.add new_id queue_to_play; *)
    | Leave -> 
        leave_queue player (fun () -> loop mess)
    | _ -> output_string sock "udalo sie cos" (fun () -> loop "wrong something")
    )
    in loop welcome_mess
    

let () = establish_server ~port:1234 proc_client


(* pownieniem wyciagnac chyba ten loop z proc client zeby bylo loop_player  *)