open Games_combined

let ( let* ) = Lwt.bind

type player_id = int
type session_id = int
type regular_session = {player_1 : player_id; player_2 : player_id; session_id : session_id; game : game_type}
type self_session = {player : player_id; session_id : session_id; game : game_type}
type session = 
  | Multiplayer of regular_session
  | Self        of self_session


module WaitingQueue : sig
  type id
  type t

  val empty  : unit -> t
  val add    : id -> t -> unit
  val remove : id -> t -> unit
  val take   : t -> id
  val length : t -> int 
  val show   : t -> string 

end with type id = int = struct
  type id = int
  type t = int Queue.t

  let empty () = Queue.create ()
  let add = Queue.push
  let remove x q = 
    let tmp = Queue.create () in
    Queue.iter (fun e -> if e <> x then Queue.add e tmp) q;
    Queue.clear q;
    Queue.iter (fun x -> Queue.add x q) tmp

  let length = Queue.length
  let take  = Queue.take

  let show q = q |> Queue.to_seq |> List.of_seq |> List.map string_of_int |> String.concat " "
end

module ClientConnections : sig
  type id
  type output
  type t

  val empty  : unit -> t
  val add    : id -> output -> t -> unit
  val iter   : (id -> output -> unit) -> t -> unit
  val filter : (id -> output -> bool) -> t -> unit
  val cardinal : t -> int
  val get : id -> t -> output
end with type id = int and type output = Lwt_io.output_channel = struct
  type id = int
  type output = Lwt_io.output_channel

  module M = Map.Make(Int) (*player_id*)

  type t = (output M.t) ref

  let empty () = ref M.empty
  let add i o cli = cli := M.add i o !cli
  let iter f cli = M.iter f !cli
  let filter f cli = cli := M.filter f !cli
  let cardinal cli = M.cardinal !cli
  let get i cli = M.find i !cli
end

module Sessions : sig 
  type t
  val empty : unit -> t
  val add   : session_id -> session -> t -> unit
  val remove : session_id -> t -> unit
  val find_opt : session_id -> t -> session option
end = struct
  module M = Map.Make(Int) (*session_id*)
  type t = (session M.t) ref
  let empty () = ref M.empty
  let add s_id sess sessions = sessions := M.add s_id sess !sessions
  let remove s_id sessions = sessions := M.remove s_id !sessions
  let find_opt s_id sessions = M.find_opt s_id !sessions
end



let sessions = Sessions.empty ()

let client_session = Hashtbl.create 32

let client_outputs = ClientConnections.empty ()

let regular_queue  = WaitingQueue.empty ()
let chess960_queue = WaitingQueue.empty ()
let fog_queue      = WaitingQueue.empty ()

let id_count = ref 0
let session_count = ref 0

(* 
sessions       - session_id -> session
client_session - client_id -> session_id
client_outputs - client_id -> output socket
waiting_queue - queue for players in a queue
*)

let get_session_id () = 
  incr session_count ;
  !session_count 

(** returns session by session_id *)
let session_by_id (s_id : session_id) : session = 
  match Sessions.find_opt s_id sessions with
  | None -> 
    Lwt.async (fun () -> Lwt_io.printl ("Session not found" ^ string_of_int s_id));
    failwith "blad"
  | Some s -> s

(** returns session_id and session by player_id*)
let session_by_player_id (p : player_id) : session_id * session = 
  match Hashtbl.find_opt client_session p with
  | None -> 
    Lwt.async (fun () -> Lwt_io.printl ("Session not found for player: " ^ string_of_int p));
    failwith "Session missing"
  | Some s_id -> s_id, session_by_id s_id

let session_id_by_player_id_opt (p : player_id) : session_id option = Hashtbl.find_opt client_session p
(** returns output socket by player_id*)
let output_by_id (p_id : player_id) : Lwt_io.output_channel = 
  ClientConnections.get p_id client_outputs

let game_from_session (s : session) : game_type = 
  match s with
  | Multiplayer s -> s.game
  | Self s        -> s.game


let update_session (s_id : session_id) (g : game_type) : unit = 
  let session = session_by_id s_id in 
  let session = match session with
  | Multiplayer s -> Multiplayer {s with game = g}
  | Self s        ->  Self {s with game = g}
  in
  
  Sessions.add s_id session sessions 


let remove_output (player_id : player_id) = 
  (*Safe to do so even if player_id isn't in the client_outputs *)

  ClientConnections.filter (fun id' _ -> id' <> player_id) client_outputs;
  Lwt.async (fun () -> Lwt_io.printl ("Client with id : " ^ string_of_int player_id ^ " disconnected"))
  
let remove_session (s_id : session_id) : unit = 
  try
  let session = session_by_id s_id in 
  (match session with
  | Multiplayer s -> 
    let p1,p2 = s.player_1,s.player_2 in
    let o1,o2 = output_by_id p1, output_by_id p2 in
    Lwt.async (fun () -> Lwt_io.write_line o1 ("STATUS LOBBY"));
    Lwt.async (fun () -> Lwt_io.write_line o2 ("STATUS LOBBY"));
    Hashtbl.remove client_session p1;
    Hashtbl.remove client_session p2
    (*delete session from both players maps*)

  | Self s        ->  
    let p = s.player in
    let out = output_by_id p in
    Lwt.async (fun () -> Lwt_io.write_line out ("STATUS LOBBY"));
    Hashtbl.remove client_session p
  );
  Sessions.remove s_id sessions
  with
  | _ -> 
    Lwt.async (fun () -> Lwt_io.printl ("Fail in session: " ^ string_of_int s_id));
    ()

let choose_queue game_type = 
  match game_type with
  | "REGULAR" -> regular_queue
  | "CHESS960"     -> chess960_queue
  | "FOG"     -> fog_queue
  | _ -> failwith "wrong queue type"


let try_join_game game_type = (*should take game_type*)
  let queue = choose_queue game_type in
  if WaitingQueue.length queue >= 2 then (
          let p1 = WaitingQueue.take queue in
          let p2 = WaitingQueue.take queue in

          let o1 = output_by_id p1 in
          let o2 = output_by_id p2 in

          let new_session = get_session_id () in

          let start_board = starting_board game_type in
          let new_game = Multiplayer {player_1 = p1; player_2 = p2; session_id = new_session; game = start_board} in
          let fen_board_w = game_to_visible White start_board in
          let fen_board_b = game_to_visible Black start_board in
          Sessions.add new_session new_game sessions;
          Hashtbl.add client_session p1 new_session;
          Hashtbl.add client_session p2 new_session;

          Lwt.async (fun () -> Lwt_io.write_line o1 ("START W " ^ fen_board_w));
          Lwt.async (fun () -> Lwt_io.write_line o2 ("START B " ^ fen_board_b)))

let join_self_game client_id game_type =
  let out = output_by_id client_id in

  let new_session = get_session_id () in
  let start_board = starting_board game_type in
  let new_game = Self {player = client_id; session_id = new_session; game = start_board} in

  Sessions.add new_session new_game sessions;
  Hashtbl.add client_session client_id new_session;
  let fen_board = game_to_visible White start_board in
  Lwt.async (fun () -> Lwt_io.write_line out ("START W " ^ fen_board))


let remove_waiting client_id = 
  (* always safe to do that even if client isn't in any queue *)

  List.iter (WaitingQueue.remove client_id) [regular_queue;chess960_queue;fog_queue]

let handle_disconnection client_id =
  let () =      
  match session_id_by_player_id_opt client_id with
  | None      -> Lwt.async (fun () -> Lwt_io.printl ("No game, maybe deleted"))
  | Some s_id -> 
    remove_session s_id
  in
  remove_output client_id;
  remove_waiting client_id



let handle_move client_id from go = 
  let game_ended = ref false in
  let s_id, sess = session_by_player_id client_id in
  let game = game_from_session sess in
  match make_move game from go with
    | None -> 
      let out = output_by_id client_id in 
      Lwt.async (fun () -> Lwt_io.printl "invalid move sent");
      Lwt.async (fun () -> Lwt_io.write_line out "INVALID")
    | Some g ->
      
      let mess col = 
        if is_draw g then 
          begin 
            game_ended := true;
            "DRAW" 
          end else
        if is_win g  then
          begin   
            game_ended := true;
            let c = get_winner g in 
            let won = Logic.string_of_color c in
            Printf.sprintf "WIN %s" won
          end
        else
          begin update_session s_id g;
          Lwt.async (fun () -> Lwt_io.printl (Printf.sprintf "UPDATE %s" (game_to_visible col g)));
          Printf.sprintf "UPDATE %s" (game_to_visible col g)
          end
      in
      match sess with
      | Multiplayer s -> 
        let p1,p2 = s.player_1, s.player_2 in 
        let out1,out2 = output_by_id p1, output_by_id p2 in
        Lwt.async (fun () -> Lwt_io.write_line out1 (mess White));
        Lwt.async (fun () -> Lwt_io.write_line out2 (mess Black));
        if !game_ended then remove_session s_id
        
      | Self _ -> 
       
        let out = output_by_id client_id in
        Lwt.async (fun () -> Lwt_io.write_line out (mess White));
        if !game_ended then remove_session s_id
  
let handle_select client_id pos = 
  let _, sess = session_by_player_id client_id in 
  let game = game_from_session sess in 
  let turn = get_turn game in 

  let can_select = 
    match sess with 
    | Multiplayer s -> ((client_id = s.player_1 && turn = White) || (client_id <> s.player_1 && turn = Black))
    | Self _ -> true
  in
    
  if can_select && 
    List.mem  (Logic.pos_to_int pos) (Logic.all_pieces_of_position (game_to_position game) turn) then 
    (*lets say that player1 is always white*)
    let out = output_by_id client_id in
    let to_highlight = 
      highlight_squares pos game
      |> List.fold_left (fun acc x -> acc ^ " " ^ string_of_int x ) "" in 
      
      Lwt.async (fun () -> Lwt_io.printl (Printf.sprintf "SELECT %s" pos));
      Lwt.async (fun () -> Lwt_io.printl (Printf.sprintf "HIGHLIGHT%s" to_highlight));
      Lwt.async (fun () -> Lwt_io.write_line out (Printf.sprintf "SELECT %s" pos));
      Lwt.async (fun () -> Lwt_io.write_line out (Printf.sprintf "HIGHLIGHT%s" to_highlight))
  else
     Lwt.async (fun () -> Lwt_io.printl (Printf.sprintf "Not your turn" ))

  

let handle_client client_id (input, output) =
    let rec loop () =
      let* line_opt = Lwt_io.read_line_opt input in

      match line_opt with
      | None -> 
        handle_disconnection client_id;
        Lwt.return_unit

      | Some s -> 
        begin match String.split_on_char ' ' s with
        | "SELF" :: game_type :: _ ->
          Lwt.async (fun () -> Lwt_io.printl "somebody started game with themselves");
          join_self_game client_id game_type;
          loop ()
        | "JOIN" :: game_type :: _ ->
          Lwt.async (fun () -> Lwt_io.printl ("somebody joined " ^ game_type));
          let queue = choose_queue game_type in
          WaitingQueue.add client_id queue;
          let fog,normal,c_960 =
          
          WaitingQueue.show fog_queue,
          
          WaitingQueue.show regular_queue,
          
          WaitingQueue.show chess960_queue 
        in
          Lwt.async (fun () -> Lwt_io.printl (Printf.sprintf "queues: 
          fog : %s
          normal : %s
          960 : %s" fog normal c_960));
          try_join_game game_type;

          loop ()
        | "LEAVE" :: _ ->
          Lwt.async (fun () -> Lwt_io.printl "somebody is trying to leave queue");
          remove_waiting client_id;
          loop ()
        | "SELECT" :: pos :: _ -> 
          handle_select client_id pos;
          loop ()
         
        | "MOVE" :: from :: go :: _  -> 
          handle_move client_id from go;
          loop ()
        | _ -> 
          let* () = Lwt_io.printl ("unknown command sent: " ^ s) in
          let* () = Lwt_io.write_line output ("incorrect command: " ^ s) in
          loop ()
        end
    in
    loop ()



let rec accept_connections server_socket =
    let* (client_socket, _addr) =   
    Lwt_unix.accept server_socket in
    let input =
      Lwt_io.of_fd ~mode:Lwt_io.input client_socket in
    let output =
      Lwt_io.of_fd ~mode:Lwt_io.output client_socket in

    
    let client_id = !id_count in
    id_count := 1 + !id_count;
    ClientConnections.add client_id output client_outputs;

    Lwt.async (fun () -> handle_client client_id (input, output));
    accept_connections server_socket

let start_server port =
    let sockaddr =
    Unix.(ADDR_INET (inet_addr_any, port)) in

    let server_socket =
    Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in

    Lwt_unix.setsockopt server_socket Unix.SO_REUSEADDR true;

    let* () = Lwt_unix.bind server_socket sockaddr in
    Lwt_unix.listen server_socket 10;
    let* () =
      Lwt_io.printlf "Server started on port %d" port
    in
    accept_connections server_socket


let () =
  let () = Random.self_init () in (*random seed to generate random game for Chess960*)
  let port = 1234 in
  Lwt_main.run (start_server port)