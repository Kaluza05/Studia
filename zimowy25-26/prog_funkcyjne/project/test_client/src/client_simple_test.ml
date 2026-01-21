open Graphics
open Lwt.Syntax

let width = 600
let height = 600

let tile_size = 60

let button = (200, 20, 400, 60) (* x1,y1,x2,y2 *)

let inside (x,y) (x1,y1,x2,y2) =
  x >= x1 && x <= x2 && y >= y1 && y <= y2


let draw_lobby () =
  clear_graph ();
  draw_rect 200 250 200 50;
  moveto 240 270;
  draw_string "Join queue"

let draw_board () =
  clear_graph ();
  for x = 0 to 7 do
    for y = 0 to 7 do
      if (x + y) mod 2 = 0 then set_color white else set_color black;
      fill_rect (x*tile_size) (y*tile_size) tile_size tile_size
    done
  done

let do_stuff input output = 
  let selected = ref None in

  let rec gui_loop () =
    if button_down () then (
      let x,y = mouse_pos () in

      if inside (x,y) button then (
        match !selected with
        | Some (sx,sy) ->
            let msg = Printf.sprintf "POS %d %d" sx sy in
            Lwt.async (fun () -> Lwt_io.write_line output msg)
        | None -> ()
      ) else (
        selected := Some (x,y);
        set_color red;
        fill_circle x y 4
      );

      Unix.sleepf 0.15
    );
    gui_loop ()
  in


  let rec receiver () =
    let* msg = Lwt_io.read_line input in
    match String.split_on_char ' ' msg with
    | ["POS"; xs; ys] ->
        let x = int_of_string xs in
        let y = int_of_string ys in
        set_color blue;
        fill_circle x y 4;
        receiver ()
    | _ -> receiver ()
  in  

  open_graph (Printf.sprintf " %dx%d" width height);
  set_window_title "Client";
  clear_graph ();
  draw_button ();

  Lwt.join [Lwt_preemptive.detach gui_loop (); receiver ()]



let client_setup host port =
  let* addr_info =
    Lwt_unix.getaddrinfo
      host
      (string_of_int port)
      [Unix.(AI_FAMILY PF_INET)]
  in
  let* addr =
    match addr_info with
    | [] -> Lwt.fail_with "Failed to resolve host"
    | addr :: _ -> Lwt.return addr.Unix.ai_addr
  in


  let socket =
    Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0
  in
  let* () = Lwt_unix.connect socket addr in
  let input =
    Lwt_io.of_fd ~mode:Lwt_io.input socket
  in
  let output =
    Lwt_io.of_fd ~mode:Lwt_io.output socket
  in

  do_stuff input output





(* potrzeba jeszcze recv messages gdzie przechwytujemy tą wiadomosć i ją sobie printujemy, w czesci serwera wysylamy ta wiadomosc
w cliencie ja odbiearamy i printujemy sobie *)

let () =
  let host = "127.0.0.1" and port = 1234 in
  Lwt_main.run (client_setup host port)