open Unix

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

let output_string sock str =
  let data = Bytes.of_string str in
  let len  = Bytes.length data in
  let rec loop pos =
    if pos = len then ()
    else
      let pos = pos + write sock.fd data pos (len - pos) in
      loop pos
  in
  loop 0

let rec input_char sock =
  if sock.buf_size = 0 then None
  else if sock.buf_pos = sock.buf_size then
    begin
      sock.buf_pos  <- 0;
      sock.buf_size <- read sock.fd sock.buf 0 (Bytes.length sock.buf);
      input_char sock
    end
  else
    begin
      let c = Bytes.get sock.buf sock.buf_pos in
      sock.buf_pos <- sock.buf_pos + 1;
      Some c
    end

let input_line sock =
  match input_char sock with
  | None -> None
  | Some '\n' -> Some ""
  | Some c ->
    let buf = Buffer.create 80 in
    Buffer.add_char buf c;
    let rec loop () =
      match input_char sock with
      | None | Some '\n' -> Buffer.contents buf
      | Some c ->
        Buffer.add_char buf c;
        loop ()
    in
    Some (loop ())

let close sock = close sock.fd

let establish_server ~port proc_client =
  let server_sock = socket PF_INET SOCK_STREAM 0 in
  bind server_sock (ADDR_INET(inet_addr_any, port));
  listen server_sock 5;
  let rec loop () =
    let (sock, _) = accept server_sock in
    proc_client (sock_of_fd sock);
    loop ()
  in
  loop ()
