let ( let* ) = Lwt.bind

type player_id = int
type session_id = int
type session = {player_1 : player_id; player_2 : player_id; session_id : session_id; game : Logic.game}

(* module ServerState = struct
  type t = {
    games : (game_id, Game.t) Hashtbl.t;
    clients : (Client.id, Client.output) Hashtbl.t;
    routing : (Client.id, game_id) Hashtbl.t;
  }
end *)


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

let waiting_queue = WaitingQueue.empty

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
let players_from_session (sess : session) : player_id * player_id = sess.player_1, sess.player_2

let update_session (s_id : session_id) (g : Logic.game) : unit = 
  let session = session_by_id s_id in 
  let session = {session with game = g} in
  sessions := M.add s_id session !sessions 




let try_join_game () = 
  if WaitingQueue.length waiting_queue >= 2 then (
          let p1 = WaitingQueue.take waiting_queue in
          let p2 = WaitingQueue.take waiting_queue in

          let o1 = output_by_id p1 in
          let o2 = output_by_id p2 in

          let new_session = get_session_id () in

          let new_game = {player_1 = p1; player_2 = p2; session_id = new_session; game = Logic.init_game ()} in
          let fen_board = Logic.game_to_fen new_game.game in
          sessions := M.add new_session new_game !sessions;
          Hashtbl.add client_session p1 new_session;
          Hashtbl.add client_session p2 new_session;
          (*Maybe give players the session_id theyre in to find which board to update*)
         

          Lwt.async (fun () -> Lwt_io.write_line o1 ("START W " ^ fen_board));
          Lwt.async (fun () -> Lwt_io.write_line o2 ("START B " ^ fen_board)))


(* let broadcast sender_id to_recv message =
  to_recv
  |> ClientConnections.filter (fun id _ -> id <> sender_id)
  |> ClientConnections.iter (fun _ output ->
       Lwt.async (fun () -> Lwt_io.write_line output message)
     ) *)

let handle_client client_id (input, output) =
    let rec loop () =
      let* line_opt = Lwt_io.read_line_opt input in

      match line_opt with
      | None -> 
        client_outputs := ClientConnections.filter (fun id' _ -> id' <> client_id) !client_outputs;
        Lwt_io.printl ("Client with id : " ^ string_of_int client_id ^ " disconnected")

      | Some s -> 
        begin match String.split_on_char ' ' s with
        | "JOIN" :: _ ->
          Lwt.async (fun () -> Lwt_io.printl "somebody joined");
          WaitingQueue.add client_id waiting_queue;
          try_join_game ();

          loop ()
        | "LEAVE" :: _ ->
          Lwt.async (fun () -> Lwt_io.printl "somebody is trying to leave queue");
          WaitingQueue.remove client_id waiting_queue;
          loop ()
        | "SELECT" :: pos :: _ -> 
          (* returns a list of squares to highlight *)
          (* highlight tylko podczas jego tury, to moze być po stronie klienta sprawdzane? *)
          let _, sess = session_by_player_id client_id in 
          let out = output_by_id client_id in
          Lwt.async (fun () -> Lwt_io.printl (Printf.sprintf "wants to highlight from: %s" pos));
          let to_highlight = 
            Logic.highlight_squares pos sess.game 
            |> List.fold_left (fun acc x -> acc ^ " " ^ string_of_int x ) "" in 

            Lwt.async (fun () -> Lwt_io.printl (Printf.sprintf "HIGHLIGHT%s" to_highlight));
            Lwt.async (fun () -> Lwt_io.write_line out (Printf.sprintf "HIGHLIGHT%s" to_highlight));
            loop ()
         
        | "MOVE" :: from :: go :: _  -> 
            let s_id, sess = session_by_player_id client_id in
            begin match Logic.move_opt from go sess.game with
            | None -> 
              let out = output_by_id client_id in 
              Lwt.async (fun () -> Lwt_io.printl "invalid move sent");
              Lwt.async (fun () -> Lwt_io.write_line out "INVALID")
            | Some g ->
              let p1,p2 = players_from_session sess in 
              let out1,out2 = output_by_id p1, output_by_id p2 in
              let mess = 
                match g with
                | Draw -> "DRAW"
                | Win c -> 
                  let won = Logic.string_of_color c in
                  Printf.sprintf "WIN %s" won
                | Playing _ -> 
                  update_session s_id g;
                  Lwt.async (fun () -> Lwt_io.printl (Printf.sprintf "UPDATE %s" (Logic.game_to_fen g)));
                  Printf.sprintf "UPDATE %s" (Logic.game_to_fen g)
              in
              Lwt.async (fun () -> Lwt_io.write_line out1 mess);
              Lwt.async (fun () -> Lwt_io.write_line out2 mess)
            end;
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
dodawanie osob do gry, ale teraz muszę wiedzieć w ktorej grze są, jak to odnalezc, jedno rozwiazanie
zawsze przekazywać session_id w komunikacji serwer-klient

teraz przy obsługa ruszania, od strony klienta do ogarnięcia:
- klika podczas swojej tury swoją figurę - zapisz co i podświetl gdzie można pojść
- jesli to samo pole odznacz, jesli inne zostaw, jesli poprawne rusz sie i wysloij do serwera


*)







(* server dalej może być w cps-ie tylko wyswietlanie od strony klienta trzeba zrobić czy coś takiego *)


(* posprzatać kod gdzie jakieś refy
kolejki
mapy 
 *)