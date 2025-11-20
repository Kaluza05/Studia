open Network

let proc_client sock =
  output_string sock "What is your name?\n";
  match input_line sock with
  | None ->
    close sock
  | Some name ->
    output_string sock (Printf.sprintf "Hello %s!\n" name);
    close sock

let () = establish_server ~port:1234 proc_client
