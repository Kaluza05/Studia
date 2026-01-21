let ( let* ) = Lwt.bind

(* Handles communication with a connected client by echoing received lines back. *)

let waiting_queue = Queue.create ()
let count = ref 0

module ClientConnections : sig
  type id
  type output
  type t

  val empty  : t
  val add    : id -> output -> t -> t
  val iter   : (id -> output -> unit) -> t -> unit
  val filter : (id -> output -> bool) -> t -> t
  val cardinal : t -> int
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
end
  

let client_outputs = ref ClientConnections.empty
(* tu mozna tez oddielić skladnie od implementacji *)
(* moze byc ref map na razie nie ma znaczenia *)

let broadcast sender_id to_recv message =
  to_recv
  |> ClientConnections.filter (fun id _ -> id <> sender_id)
  |> ClientConnections.iter (fun _ output ->
       Lwt.async (fun () -> Lwt_io.write_line output message)
     )

let handle_client client_id (input, output) =
    let rec loop () =
      let* line_opt = Lwt_io.read_line_opt input in

      match line_opt with
      | None -> 
        client_outputs := ClientConnections.filter (fun id' _ -> id' <> client_id) !client_outputs;
        Lwt_io.printl ("Client with id : " ^ string_of_int client_id ^ " disconnected")

      | Some line ->
        let* () = Lwt_io.printl ("number of clients : " ^ string_of_int (ClientConnections.cardinal !client_outputs)) in
        let* () = Lwt_io.printl ("sending message to all clients: " ^ line) in 

        broadcast client_id !client_outputs line;
        let* () = Lwt_io.write_line output "Confirmed, sent out" in
        loop ()
    in
    loop ()

let rec accept_connections server_socket =
    let* (client_socket, _addr) =   
    Lwt_unix.accept server_socket in
    let input =
      Lwt_io.of_fd ~mode:Lwt_io.input client_socket in
    let output =
      Lwt_io.of_fd ~mode:Lwt_io.output client_socket in

    
    let client_id = !count in
    count := 1 + !count;
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