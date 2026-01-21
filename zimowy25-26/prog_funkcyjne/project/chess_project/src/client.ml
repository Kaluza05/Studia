open Graphics


let main () =
  let host = Sys.argv.(1) in
  let port = int_of_string Sys.argv.(2) in
  let sock = Network.connect ~host ~port in

  Graphics.open_graph " 600x600";

  let rec loop () =
    (* 1. odbiór z serwera *)
    Network.input_line sock (function
      | Some msg ->
          update_board msg;
          draw_board ();
          loop ()
      | None ->
          Network.close sock
    )
  in
  loop ()