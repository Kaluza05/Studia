open Network

let proc_client sock =
  output_string sock "What is your name?\n" (fun () ->
  input_line sock (fun line_opt ->
  match line_opt with
  | None ->
    close sock;
    exit ()
  | Some name ->
    output_string sock (Printf.sprintf "Hello %s!\n" name) (fun () ->
    close sock;
    exit ())))

let () = establish_server ~port:1234 proc_client
