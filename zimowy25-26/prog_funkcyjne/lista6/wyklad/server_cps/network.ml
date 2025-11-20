open Unix

module Thr : sig
  type ans

  val exit : unit -> ans
end = struct
  type ans = unit

  let exit () = ()
end
include Thr

type sock =
  {         fd       : file_descr
  ;         buf      : bytes
  ; mutable buf_pos  : int
  ; mutable buf_size : int
  }

let sock_of_fd fd =
  { fd       = fd
  ; buf      = Bytes.create 4096
  ; buf_pos  = 1
  ; buf_size = 1
  }

let write fd data pos len cont =
  cont (write fd data pos len)

let read fd data pos len cont =
  cont (read fd data pos len)

let accept fd cont =
  cont (accept fd)

let output_string sock str cont =
  let data = Bytes.of_string str in
  let len  = Bytes.length data in
  let rec loop pos cont =
    if pos = len then cont ()
    else
      write sock.fd data pos (len - pos) (fun n ->
      let pos = pos + n in
      loop pos cont)
  in
  loop 0 cont

let rec input_char sock cont =
  if sock.buf_size = 0 then cont None
  else if sock.buf_pos = sock.buf_size then
    begin
      sock.buf_pos  <- 0;
      read sock.fd sock.buf 0 (Bytes.length sock.buf) (fun n ->
      sock.buf_size <- n;
      input_char sock cont)
    end
  else
    begin
      let c = Bytes.get sock.buf sock.buf_pos in
      sock.buf_pos <- sock.buf_pos + 1;
      cont (Some c)
    end

let input_line sock cont =
  input_char sock (function
  | None -> cont None
  | Some '\n' -> cont (Some "")
  | Some c ->
    let buf = Buffer.create 80 in
    Buffer.add_char buf c;
    let rec loop cont =
      input_char sock (function
      | None | Some '\n' -> cont (Buffer.contents buf)
      | Some c ->
        Buffer.add_char buf c;
        loop cont)
    in
    loop (fun x -> cont (Some x)))

let close sock = close sock.fd

let establish_server ~port proc_client =
  let server_sock = socket PF_INET SOCK_STREAM 0 in
  bind server_sock (ADDR_INET(inet_addr_any, port));
  listen server_sock 5;
  let rec loop () =
    accept server_sock (fun (sock, _) ->
    proc_client (sock_of_fd sock);
    loop ())
  in
  loop ()
