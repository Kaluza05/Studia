open Logic
open Tsdl
open Tsdl_ttf
open Tsdl_image
open Lwt.Syntax
open Lwt.Infix


let tile_size = 60
let board_size = 8 * tile_size

 (* x1,y1,x2,y2 *)
let regular_button     = (117, 70, 356, 112)
let sandbox_button     = (18, 398, 225, 435)
let sandbox_960_button = (253, 398, 459, 435)
let chess_960_button   = (18, 318, 225, 358)
let fog_button         = (253, 318, 459, 358)
let leave_button       = (132, 408, 360, 456)

let inside (x,y) (x1,y1,x2,y2) =
  x >= x1 && x <= x2 && y >= y1 && y <= y2

let outside_window (x,y) = 
  x < 0 || y < 0 || x > board_size || y > board_size

let load_texture renderer path =
  match Image.load_texture renderer path with
  | Error (`Msg e) -> 
    Sdl.log "Load texture error: %s" e;
    failwith "blad w ladowaniu"
  | Ok tex -> tex

let draw_texture ?(resize : (int * int) option) renderer texture (x, y) =
  match Sdl.query_texture texture with
  | Error (`Msg e) -> Sdl.log "Query texture error: %s" e
  | Ok (_format, _access, (w, h)) ->
      let w,h = begin match resize with
      | None -> w,h
      | Some(w,h) -> w,h
      end
      in
      let dst_rect = Sdl.Rect.create ~x ~y ~w ~h in
      Sdl.render_copy renderer texture ~dst:dst_rect |> ignore
      
let get_sqare_color (x,y) = 
  if (x + y) mod 2 = 0 then 
    (235, 236, 208, 255) (*White square*)
  else
    (119, 149, 86, 255)  (*Black square*)
let draw_board renderer =
  (*Clear canvas to white*)

  Sdl.set_render_draw_color renderer 255 255 255 255 |> ignore;
  Sdl.render_clear renderer |> ignore;

  for x = 0 to 7 do
    for y = 0 to 7 do
      let r,g,b,a = get_sqare_color (x,y) in
      Sdl.set_render_draw_color renderer r g b a |> ignore;

      let rect = Sdl.Rect.create ~x:(x*tile_size) ~y:(y*tile_size) ~w:tile_size ~h:tile_size in
      Sdl.render_fill_rect renderer (Some rect) |> ignore
    done
  done


type state = 
  | InLobby
  | InQueue
  | InGame

type ui_board = {
  board : Logic.position;
  selected : string option;
  to_highlight : int list;
  draw_perspective : Logic.color}



let screen_pos_to_square (col : Logic.color) (x,y) = 
  let r,c = x / tile_size, y/tile_size in 
  match col with
  | White -> flatten_pos c r
  | Black -> flatten_pos (7-c) (7-r)

let square_pos_to_screen (col : color) (p : int) = 
  let r,c = unflatten_pos p in
  match col with
  | White -> tile_size * c, tile_size * r
  | Black -> tile_size * (7-c), tile_size * (7-r)

let get_square (col : Logic.color) (x,y) = screen_pos_to_square col (x,y) |> int_to_pos




let my_game  = ref None

let update_selected sel = 
  my_game := 
  match !my_game with
  | None -> None
  | Some g -> Some ({g with selected = sel})

let get_game () = 
  match !my_game with
  | None -> failwith "wanted game but got none, not possible"
  | Some g -> g


let init_board_ui init_board my_color = 
  my_game := Some {board = init_board ; selected = None; to_highlight = []; draw_perspective = my_color}


let update_board fen = 
  match !my_game with
  | None -> failwith "game is not initialized"
  | Some g ->
    let new_board = Logic.fen_to_board fen in
    my_game := Some {g with board = new_board; selected = None; to_highlight = []} 


let draw_selected renderer ?(color = (245, 246, 130)) (col : Logic.color) (p : int) =
  let x, y = square_pos_to_screen col p in

  let r, g, b = color in
  Sdl.set_render_draw_color renderer r g b 128 |> ignore;

  let rect = Sdl.Rect.create ~x ~y ~w:tile_size ~h:tile_size in
  Sdl.render_fill_rect renderer (Some rect) |> ignore

let figure_to_path (f : Logic.color * piece) : string = 
  match f with
  | White, Pawn   -> "src/pieces/white-pawn.png"
  | White, Rook   -> "src/pieces/white-rook.png"
  | White, Knight -> "src/pieces/white-knight.png"
  | White, Bishop -> "src/pieces/white-bishop.png"
  | White, King   -> "src/pieces/white-king.png"
  | White, Queen  -> "src/pieces/white-queen.png"
  | Black, Pawn   -> "src/pieces/black-pawn.png"
  | Black, Rook   -> "src/pieces/black-rook.png"
  | Black, Knight -> "src/pieces/black-knight.png"
  | Black, Bishop -> "src/pieces/black-bishop.png"
  | Black, King   -> "src/pieces/black-king.png"
  | Black, Queen  -> "src/pieces/black-queen.png"

let draw_figure renderer (col : Logic.color) (pos : int) (f : figure) = 
  let x,y = square_pos_to_screen col pos in 
  let piece_path = figure_to_path f in 
  let piece_tex = load_texture renderer piece_path in
  draw_texture renderer piece_tex (x,y) ~resize:(tile_size,tile_size)


let draw_lobby renderer =
  Sdl.set_render_draw_color renderer 255 255 255 255 |> ignore;
  Sdl.render_clear renderer |> ignore;
  let slop_path = "src/pieces/lobby_ai_slop.png" in
  let slop_image = load_texture renderer slop_path in
  draw_texture renderer slop_image (0,0) ~resize:(board_size,board_size)

let draw_queue renderer =
  Sdl.set_render_draw_color renderer 255 255 255 255 |> ignore;
  let slop_path = "src/pieces/queue_ai_slop.png" in
  let slop_image = load_texture renderer slop_path in
  draw_texture renderer slop_image (0,0) ~resize:(board_size,board_size)


let draw_game renderer g =
  let col,selected,to_highlight = g.draw_perspective, g.selected, g.to_highlight in
  draw_board renderer;
  begin match selected with
  | None -> ()
  | Some p -> p |> pos_to_int |> draw_selected renderer col ~color:(245, 246, 130)
  end;
  List.iter (draw_selected renderer col) to_highlight;
  List.iteri (fun pos f -> 
    match f with
    | None -> ()
    | Some f -> draw_figure renderer col pos f) 
    g.board

let draw renderer state game_ui = 
  begin match state with
  | InLobby -> draw_lobby renderer
  | InQueue -> draw_queue renderer
  | InGame -> 
    match game_ui with 
    | None -> failwith "in game but no game to draw"
    | Some g -> 
      draw_game renderer g
  end;
  
  Sdl.render_present renderer


let new_highlight xs = 
  my_game := 
  match !my_game with
  | None -> None
  | Some g -> Some ({g with to_highlight = xs})

let () = 
  Lwt.async_exception_hook := (fun exn ->
  print_endline ("LWT ASYNC EXN: " ^ Printexc.to_string exn))


let do_stuff input output = 
  let running = ref true in
  let outgoing = Lwt_mvar.create_empty () in
  let add_to_send mess = Lwt_mvar.put outgoing mess in
  let state = ref InLobby in


  let handle_mouse_click (x,y) =
  Lwt.catch (fun () ->

      if outside_window (x,y) then Lwt.return_unit else 
      match !state with
      | InLobby when inside (x,y) sandbox_960_button ->
        add_to_send "SELF CHESS960"
      | InLobby when inside (x,y) sandbox_button ->
        add_to_send "SELF REGULAR"
      | InLobby when inside (x,y) regular_button ->
          state := InQueue;
          add_to_send "JOIN REGULAR"
      | InLobby when inside (x,y) fog_button ->
        state := InQueue;
        add_to_send "JOIN FOG"
      | InLobby when inside (x,y) chess_960_button ->
        state := InQueue;
        add_to_send "JOIN CHESS960"
      | InQueue when inside (x,y) leave_button -> 
          state := InLobby;
          add_to_send "LEAVE"
      | InGame ->
        begin match !my_game with
        | None -> 
          Lwt.return_unit
        | Some g -> 
            let square = get_square (g.draw_perspective) (x,y) in
            match g.selected with
            | None -> 
              add_to_send (Printf.sprintf "SELECT %s" square)
            | Some p ->
              begin match p with
              | _ when p = square -> 
                update_selected None;
                new_highlight [];
                Lwt.return_unit
                (*deselect the square*)
              | _ -> 
                add_to_send (Printf.sprintf "MOVE %s %s" p square)
              
              end
        end
      | _ -> 
        add_to_send "some other case"

    )

     (fun ex ->
      Lwt_io.printl ("EXCEPTION: " ^ Printexc.to_string ex)
    )

  in

  let rec gui_loop renderer window = 

    let handle_events () =
    let open Sdl.Event in
    let event = create () in
      let rec loop () = 
      if Sdl.poll_event (Some event) then 
        begin match get event typ with
        | t when t = quit -> running := false
        | t when t = mouse_button_down ->
            let x = get event mouse_button_x in
            let y = get event mouse_button_y in
            handle_mouse_click (x,y) |> ignore
        | _ -> ()
        ;
        loop ()
        end
      in
      loop ()
    in
    handle_events ();

    draw renderer !state !my_game;
      
    if !running then 
      Lwt_unix.sleep 0.016 >>= fun () -> gui_loop renderer window
    else 
      begin 
      Sdl.destroy_renderer renderer;
      Sdl.destroy_window window;
      Sdl.quit ();
      Lwt.return_unit
      end

  in

  let rec sender () = 
    let* msg = Lwt_mvar.take outgoing in
    let* () = Lwt_io.write_line output msg in 
    sender ()
    
  in

  let rec receiver () =
    let* msg = Lwt_io.read_line input in
    match String.split_on_char ' ' msg with
    | "START" :: c :: fen :: _ ->
      state := InGame;
      let my_color = match c with| "W" -> White| "B" -> Black| _ -> failwith "wrong message from server"
      in
      Lwt.async (fun () -> Lwt_io.printl ("my color is: " ^ c));
      init_board_ui (Logic.fen_to_board fen) my_color;
      Lwt.async (fun () -> Lwt_io.printl ("initialized ui?"));
      receiver ()
    | "START" :: _ ->
      Lwt.async (fun () -> Lwt_io.printl ("incomplete START "));
      receiver ()
    | "UPDATE" :: fen :: _ -> 
      update_board fen;
      update_selected None;
      new_highlight [];
      receiver ()
    | "INVALID" :: _ -> 
      receiver ()
    | "SELECT" :: pos :: _ ->
      update_selected (Some pos);
      receiver ()
    | "HIGHLIGHT" :: to_highlight ->
      new_highlight (List.map int_of_string to_highlight);
      receiver ()
    | "WIN" :: c :: _ -> 
      let g = get_game () in
      let mess = if Logic.string_of_color g.draw_perspective = c then "You won." else "You lost." in
      Lwt.async (fun () -> Lwt_io.printl mess);
      state := InLobby;
      my_game := None;
      receiver ()
    | "DRAW" :: _ -> 
      Lwt.async (fun () -> Lwt_io.printl "Game ended in a draw");
      state := InLobby;
      my_game := None;
      receiver ()
    | "STATUS" :: "LOBBY" :: _ ->
      state := InLobby;
      my_game := None;
      receiver ()
    | _ -> receiver ()
  in  

  
  match Sdl.init Sdl.Init.video with
  | Error (`Msg e) -> failwith e
  | Ok () ->

  match Ttf.init () with
  | Error (`Msg e) -> failwith ("TTF init error: " ^ e)
  | Ok () ->

  match Sdl.create_window ~w:board_size ~h:board_size "Client" Sdl.Window.opengl with
  | Error (`Msg e) -> failwith e
  | Ok window ->

  match Sdl.create_renderer window ~index:(-1) ~flags:Sdl.Renderer.accelerated with
  | Error (`Msg e) -> failwith e
  | Ok renderer ->

    Sdl.set_hint Sdl.Hint.render_scale_quality "linear" |> ignore;


    
    let gui = gui_loop renderer window in
    let recv = receiver () in
    let send = sender () in

    (*stop the loop after quitting*)
    Lwt.pick [gui] >>= fun () ->
    Lwt.cancel recv;
    Lwt.cancel send;
    Lwt.return_unit




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


let () =
  let host = "127.0.0.1" and port = 1234 in
  Lwt_main.run (client_setup host port)