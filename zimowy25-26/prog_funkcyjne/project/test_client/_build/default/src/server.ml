let ( let* ) = Lwt.bind

type game_type = 
  | Regular of Logic.game
  | Chess960 of int(*of Chess960.game*)
  | FogOfWar of int (*of Fogofwar.game*)
type player_id = int
type session_id = int
type regular_session = {player_1 : player_id; player_2 : player_id; session_id : session_id; game : game_type}
type self_session = {player : player_id; session_id : session_id; game : game_type}
type session = 
  | Multiplayer of regular_session
  | Self        of self_session

(* module ServerState = struct
  type t = {
    games : (game_id, Game.t) Hashtbl.t;
    clients : (Client.id, Client.output) Hashtbl.t;
    routing : (Client.id, game_id) Hashtbl.t;
  }
end *)

(* 
Queues different foe each type

*)

module WaitingQueue : sig
  type id
  type t

  val empty  : t
  val add    : id -> t -> unit
  val remove : id -> t -> unit
  val take   : t -> id
  val length : t -> int 

end with type id = int = struct
  type id = int
  type t = int Queue.t

  let empty = Queue.create ()
  let add = Queue.push
  let remove x q = 
    let tmp = Queue.create () in
    Queue.iter (fun e -> if e <> x then Queue.add e tmp) q;
    Queue.clear q;
    Queue.iter (fun x -> Queue.add x q) tmp

  let length = Queue.length
  let take  = Queue.take
end

module ClientConnections : sig
  type id
  type output
  type t

  val empty  : t
  val add    : id -> output -> t -> t
  val iter   : (id -> output -> unit) -> t -> unit
  val filter : (id -> output -> bool) -> t -> t
  val cardinal : t -> int
  val get : id -> t -> output
end with type id = int and type output = Lwt_io.output_channel = struct
   type id = int
  type output = Lwt_io.output_channel

  module M = Map.Make(Int)

  type t = output M.t

  let empty = M.empty
  let add = M.add
  let iter = M.iter
  let filter = M.filter
  let cardinal = M.cardinal
  let get = M.find
end

(* consistent moze zeby było i same hashtable *)



module M = Map.Make(Int) (*session_id*)

let sessions = ref M.empty

let client_session = Hashtbl.create 32

let client_outputs = ref ClientConnections.empty

let regular_queue  = WaitingQueue.empty
let chess960_queue = WaitingQueue.empty
let fog_queue      = WaitingQueue.empty

let id_count = ref 0
let session_count = ref 0

(* 
sessions       - session_id -> session
client_session - client_id -> session_id
client_outputs - client_id -> output socket
waiting_queue - queue for players in a queue

*)

(* tu mozna tez oddielić skladnie od implementacji *)
(* moze byc ref map na razie nie ma znaczenia *)
let get_session_id () = 
  incr session_count ;
  !session_count 

(** returns session by session_id *)
let session_by_id (s_id : session_id) : session = M.find s_id !sessions

(** returns session_id and session by player_id*)
let session_by_player_id (p : player_id) : session_id * session = 
  let s_id = p |> Hashtbl.find client_session in 
  s_id, session_by_id s_id

(** returns output socket by player_id*)
let output_by_id (p_id : player_id) : Lwt_io.output_channel = ClientConnections.get p_id !client_outputs

(** returns both players from the given session*)
let players_from_regular_session (sess : regular_session) : player_id * player_id = sess.player_1, sess.player_2

let game_from_session (s : session) : game_type = 
  match s with
  | Multiplayer s -> s.game
  | Self s        -> s.game

let game_type_by_game (g : game_type) = 
match g with
  | Regular _  -> "REGULAR"
  | Chess960 _ -> "CHESS960"
  | FogOfWar _ -> "FOG"

let game_type_by_player_id (c_id : player_id) : string = 
  let _,sess = session_by_player_id c_id in
  sess |> game_from_session |> game_type_by_game


    

let update_session (s_id : session_id) (g : game_type) : unit = 
  let session = session_by_id s_id in 
  let session = match session with
  | Multiplayer s -> Multiplayer {s with game = g}
  | Self s        ->  Self {s with game = g}
  in
  
  sessions := M.add s_id session !sessions 

let choose_queue game_type = 
  match game_type with
  | "REGULAR" -> regular_queue
  | "CHESS960"     -> chess960_queue
  | "FOG"     -> fog_queue
  | _ -> failwith "wrong queue type"

let starting_board game_type = 
  match game_type with
  | "REGULAR"  -> Regular (Logic.init_game ())
  | "CHESS960" -> Regular (Logic.init_game ())
  | "FOG"      -> Regular (Logic.init_game ())
  | _ -> failwith "wrong queue type"

let make_move (g : game_type) from go = 
  match g with
  | Regular  g -> 
    begin match g |> Logic.move_opt from go with
    | None -> None
    | Some g -> Some (Regular g)
    end
  | Chess960 g -> failwith "a" (*Chess960.game_to_board |> Logic.game_to_fen*)
  | FogOfWar g -> failwith "a" (*Logic.game_to_board |> Logic.game_to_fen*)

let game_to_fen (g : game_type) = 
  match g with
  | Regular  g -> g |> Logic.game_to_position |> Logic.game_to_fen
  | Chess960 g -> failwith "a" (*Chess960.game_to_board |> Logic.game_to_fen*)
  | FogOfWar g -> failwith "a" (*Logic.game_to_board |> Logic.game_to_fen*)

let is_draw (g : game_type) : bool = 
  match g with
  | Regular Draw -> true
  | Regular _ ->  false
  | _ -> failwith "not implemented is_draw"
let is_win (g : game_type) : bool = 
  match g with
  | Regular (Win _) -> true
  | Regular _ -> false
  | _ -> failwith "not implemented is_win"

let get_winner (g : game_type) : Logic.color = 
  match g with
  | Regular (Win c) -> c
  | _ -> failwith "not implemented get_winner"

let highlight_squares (pos : string) (g : game_type) = 
  match g with
  | Regular g -> Logic.highlight_squares pos g
  | _ -> failwith "aa"


  

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
          let fen_board = game_to_fen start_board in
          sessions := M.add new_session new_game !sessions;
          Hashtbl.add client_session p1 new_session;
          Hashtbl.add client_session p2 new_session;
          (*Maybe give players the session_id theyre in to find which board to update*)
         

          Lwt.async (fun () -> Lwt_io.write_line o1 ("START W " ^ fen_board));
          Lwt.async (fun () -> Lwt_io.write_line o2 ("START B " ^ fen_board)))

let join_self_game client_id game_type = (*should take game_type*)
  let out = output_by_id client_id in

  let new_session = get_session_id () in
  let start_board = starting_board game_type in
  let new_game = Self {player = client_id; session_id = new_session; game = start_board} in

  sessions := M.add new_session new_game !sessions;
  Hashtbl.add client_session client_id new_session;
  (*Maybe give players the session_id theyre in to find which board to update*)
  let fen_board = game_to_fen start_board in
  Lwt.async (fun () -> Lwt_io.write_line out ("START W " ^ fen_board))


let handle_move client_id from go = 
  let s_id, sess = session_by_player_id client_id in
  let game = game_from_session sess in
  match make_move game from go with
    | None -> 
      let out = output_by_id client_id in 
      Lwt.async (fun () -> Lwt_io.printl "invalid move sent");
      Lwt.async (fun () -> Lwt_io.write_line out "INVALID")
    | Some g ->
      
      let mess = 
        if is_draw g then "DRAW" else
        if is_win g  then
        let c = get_winner g in 
        let won = Logic.string_of_color c in
        Printf.sprintf "WIN %s" won
        else
          (update_session s_id g;
          Lwt.async (fun () -> Lwt_io.printl (Printf.sprintf "UPDATE %s" (game_to_fen g)));
          Printf.sprintf "UPDATE %s" (game_to_fen g))
      in
      match sess with
      | Multiplayer s -> 
        let p1,p2 = players_from_regular_session s in 
      let out1,out2 = output_by_id p1, output_by_id p2 in
      Lwt.async (fun () -> Lwt_io.write_line out1 mess);
      Lwt.async (fun () -> Lwt_io.write_line out2 mess)
      | Self _ -> 
      let out = output_by_id client_id in
      Lwt.async (fun () -> Lwt_io.write_line out mess)
  

            

let handle_client client_id (input, output) =
    let rec loop () =
      let* line_opt = Lwt_io.read_line_opt input in

      match line_opt with
      | None -> 
        client_outputs := ClientConnections.filter (fun id' _ -> id' <> client_id) !client_outputs;
        Lwt_io.printl ("Client with id : " ^ string_of_int client_id ^ " disconnected")

      | Some s -> 
        begin match String.split_on_char ' ' s with
        | "SELF" :: game_type :: _ ->
          Lwt.async (fun () -> Lwt_io.printl "somebody started game with themselves");
          join_self_game client_id game_type;
          loop ()
        | "JOIN" :: game_type :: _ ->
          Lwt.async (fun () -> Lwt_io.printl "somebody joined");
          let queue = choose_queue game_type in
          WaitingQueue.add client_id queue;
          try_join_game game_type;

          loop ()
        | "LEAVE" :: _ ->
          Lwt.async (fun () -> Lwt_io.printl "somebody is trying to leave queue");
          let game_type =  game_type_by_player_id client_id in
          let queue = choose_queue game_type in
          WaitingQueue.remove client_id queue;
          loop ()
        | "SELECT" :: pos :: _ -> 
          (* returns a list of squares to highlight *)
          (* highlight tylko podczas jego tury, to moze być po stronie klienta sprawdzane? *)
          let _, sess = session_by_player_id client_id in 
          let out = output_by_id client_id in
          Lwt.async (fun () -> Lwt_io.printl (Printf.sprintf "wants to highlight from: %s" pos));
          let game = game_from_session sess in 
          let to_highlight = 
            highlight_squares pos game
            |> List.fold_left (fun acc x -> acc ^ " " ^ string_of_int x ) "" in 

            Lwt.async (fun () -> Lwt_io.printl (Printf.sprintf "HIGHLIGHT%s" to_highlight));
            Lwt.async (fun () -> Lwt_io.write_line out (Printf.sprintf "HIGHLIGHT%s" to_highlight));
            loop ()
         
        | "MOVE" :: from :: go :: _  -> 
          handle_move client_id from go;
            
          (* validate move *)
          (* check if player is in game and if so find the player that's in the game with him
            or if were here assume that he is in a game because somebody sent him that message
              no here the player sends it so no*)
          
          (* M bedzie oznaczało ruch wysyłamy do drugiego klienta oba ruchy i niech sobie przesunie je na planszy 
          po drodze serwer tez moze cos sam z tym zrobić, jak zapisywać przebieg partii itp.*)
          
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
    client_outputs := ClientConnections.add client_id output !client_outputs;

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
  let port = 1234 in
  Lwt_main.run (start_server port)



(* 
jesli pole na ktorym stoi nasza figura to zmien zaznaczenie
chyba lepiej trzymac takie info jak kolor gracza na serwerze i tam przerobić kliknięcie
dodac mozna rozne tryby gry i osobne kolejki na nie

*)


(* 
dodac inne tryby, mozliwosc grania na siebie
przcisk rezygnacji
w grze ze sobą możliwość undo


*)




(* server dalej może być w cps-ie tylko wyswietlanie od strony klienta trzeba zrobić czy coś takiego *)


(* posprzatać kod gdzie jakieś refy
kolejki
mapy 
 *)