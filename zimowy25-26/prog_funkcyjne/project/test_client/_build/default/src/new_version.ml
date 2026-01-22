open Logic
open Tsdl
open Tsdl_ttf
open Tsdl_image
open Lwt.Syntax
open Lwt.Infix


let tile_size = 60
let board_size = 8 * tile_size


let join_button = (200, 200, 400, 300) (* x1,y1,x2,y2 *)
let leave_button = (200, 200, 400, 300) (* x1,y1,x2,y2 *)

let inside (x,y) (x1,y1,x2,y2) =
  x >= x1 && x <= x2 && y >= y1 && y <= y2

let outside_window (x,y) = 
  x < 0 || y < 0 || x > board_size || y > board_size


let load_font path = 
  match Ttf.open_font path 24 with
  | Error (`Msg e) -> failwith ("Cannot load font: " ^ e)
  | Ok f -> f


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
    (235, 236, 208, 255) (*pola biale*)
  else
    (119, 149, 86, 255)  (* pola czarne *)
let draw_board renderer =
  (* Czyścimy ekran na biało *)
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


let draw_text renderer font (x, y) text =
  match Ttf.render_text_solid font text (Sdl.Color.create ~r:0 ~g:0 ~b:0 ~a:255) with
  | Error (`Msg e) -> Sdl.log "TTF render error: %s" e
  | Ok surface ->
      match Sdl.create_texture_from_surface renderer surface with
      | Error (`Msg e) ->
          Sdl.log "Create texture error: %s" e;
          Sdl.free_surface surface
      | Ok texture ->
          let w, h = Sdl.get_surface_size surface in
          let dst_rect = Sdl.Rect.create ~x ~y ~w ~h in
          Sdl.render_copy renderer texture ~dst:dst_rect |> ignore;
          Sdl.destroy_texture texture;
          Sdl.free_surface surface


let draw_rect renderer (x1, y1, x2, y2) =
  let rect = Sdl.Rect.create ~x:x1 ~y:y1 ~w:(x2-x1) ~h:(y2-y1) in
  Sdl.set_render_draw_color renderer 0 0 0 255 |> ignore;  (* czarny *)
  Sdl.render_draw_rect renderer (Some rect) |> ignore

let draw_lobby renderer =
  let font = load_font "src/pieces/ARIAL.TTF" in
  Sdl.set_render_draw_color renderer 255 255 255 255 |> ignore; (* białe tło *)
  Sdl.render_clear renderer |> ignore;
  draw_rect renderer join_button;
  let x1, y1, x2, y2 = join_button in
  draw_text renderer font (x1 + (x2-x1)/4, (y1+y2)/2) "Join queue"

let draw_queue renderer =
  let font = load_font "src/pieces/ARIAL.TTF" in
  Sdl.set_render_draw_color renderer 255 255 255 255 |> ignore;
  Sdl.render_clear renderer |> ignore;
  draw_rect renderer leave_button;
  let x1, y1, x2, y2 = leave_button in
  draw_text renderer font (x1 + (x2-x1)/4, (y1+y2)/2 ) "Leave queue"

type state = 
  | InLobby
  | InQueue
  | InGame

type ui_board = {
  board : Logic.position;
  selected : string option;
  to_highlight : int list;
  my_color : Logic.color;
  turn : Logic.color}



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
  my_game := Some {board = init_board ; selected = None; to_highlight = []; my_color = my_color; turn = White}


let update_board fen = 
  (* move validation was done on the server side, here we only move *)
  match !my_game with
  | None -> failwith "game is not inisialized"
  | Some g ->
    let new_board = Logic.fen_to_board fen in
    let new_turn = switch_turn g.turn in
    (* Lwt.async (fun () -> Lwt_io.printl ("new turn is : " ^ (match new_turn with White -> "White" | Black -> "Black"))); *)
  my_game := Some {board = new_board; selected = None; to_highlight = []; my_color = g.my_color; turn = new_turn} 


let draw_selected renderer ?(color = (245, 246, 130)) (col : Logic.color) (p : int) =
  let x, y = square_pos_to_screen col p in

  (* Lwt.async (fun () ->
    Lwt_io.printl (Printf.sprintf "pozycja: %d %d" x y)
  ); *)

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

let draw_game renderer g =
  let col,selected,to_highlight = g.my_color, g.selected, g.to_highlight in
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
    (* wszystko zamienić na wspolrzedne, potrzeba jeszcze kolor gracza *)

let string_of_state state = 
  match state with
  | InLobby -> "InLobby"
  | InQueue -> "InQueue"
  | InGame  -> "InGame"

let string_of_mygame game = 
  match game with
  | None -> "None"
  | Some _ -> "Some game"

let show_selected sel = 
  match sel with
  | None ->  "None"
  | Some s -> s

let new_highlight xs = 
  my_game := 
  match !my_game with
  | None -> None
  | Some g -> Some ({g with to_highlight = xs})

let () = 
  Lwt.async_exception_hook := (fun exn ->
  print_endline ("LWT ASYNC EXN: " ^ Printexc.to_string exn))


let close_window () = 
let open Sdl.Event in
  let event = create () in
  if Sdl.poll_event (Some event) then
    match get event typ with
    | t when t = quit -> true
    | _ -> false
  else
    false

let mouse_click () =
  let open Sdl.Event in
  let event = create () in
  if Sdl.poll_event (Some event) then
    match get event typ with
    | t when t = mouse_button_down -> 
        let x = get event mouse_button_x in 
        let y = get event mouse_button_y in
        Some (x,y)
    | _ -> None
  else
    None




let do_stuff input output = 
  let running = ref true in
  let outgoing = Lwt_mvar.create_empty () in
  let add_to_send mess = Lwt_mvar.put outgoing mess in
  let state = ref InLobby in


  let handle_mouse_click (x,y) = 
  Lwt.catch (fun () ->

      (* Lwt.async (fun () -> Lwt_io.printl (Printf.sprintf "klikam tutaj: %d %d" x y)); *)
      if outside_window (x,y) then Lwt.return_unit else 
      match !state with
      | InLobby when inside (x,y) join_button ->
          state := InQueue;
          add_to_send "JOIN"
      | InQueue when inside (x,y) leave_button -> 
          state := InLobby;
          add_to_send "LEAVE"
      | InGame ->
        begin match !my_game with
        | None -> 
          (* Lwt.async (fun () -> Lwt_io.printl "this should be impossible case"); *)
          Lwt.return_unit
        | Some g -> 
          (* Lwt.async (fun () -> 
            if g.turn = g.my_color 
            then  Lwt.return_unit
            (* else Lwt_io.printl "not my turn" *)
            ); *)
          if g.turn <> g.my_color then Lwt.return_unit 
          else
            let square = get_square (g.my_color) (x,y) in
            (* Lwt.async (fun () -> Lwt_io.printl ("clicked square: " ^ square));
            Lwt.async (fun () -> Lwt_io.printl ("currently selected: " ^ show_selected g.selected));
            
            Lwt.async (fun () -> Lwt_io.printl ("current board:\n" ^ show_fake_board g.board)); *)
            match g.selected with
            | None -> 
              if is_my_color g.board g.my_color square then 
                begin
                update_selected (Some(square));
                add_to_send (Printf.sprintf "SELECT %s" square)
                end
              else Lwt.return_unit
              (* check if its our figure and select it *)
              (* make it the new selected *)
            | Some p ->
              begin match p with
              | _ when p = square -> 
                update_selected None;
                new_highlight [];
                Lwt.return_unit
                (*deselect the square*)
              | _ when not (List.mem (Logic.pos_to_int square) g.to_highlight) -> Lwt.return_unit (*do nothing cuz click is in the wrong place*)
              | _ -> 
                update_selected None; (*deselect after making a move*)
                new_highlight [];
                (* Lwt.async (fun () -> Lwt_io.printl  (Printf.sprintf "debug MOVE %s %s" p square)); *)
                add_to_send (Printf.sprintf "MOVE %s %s" p square)
              end
               (* "send it to the server and see if it's a correct move by server answer
                but we can do it ourselves now and server will double check us" 
               if p = square then make selected none*)
        end
      | _ -> 
        (* Lwt.async (fun () -> Lwt_io.printl (Printf.sprintf "pos: %d %d" x y));
        Lwt.async (fun () -> Lwt_io.printl (Printf.sprintf "some other case, game state: %s" (string_of_state !state))); *)
        add_to_send "some other case"

    )

     (fun ex ->
      Lwt_io.printl ("EXCEPTION: " ^ Printexc.to_string ex)
    )

  in

  let rec gui_loop renderer = 

    let rec handle_events () =
    let open Sdl.Event in
    let event = create () in
    if Sdl.poll_event (Some event) then begin
      match get event typ with
      | t when t = quit -> running := false
      | t when t = mouse_button_down ->
          let x = get event mouse_button_x in
          let y = get event mouse_button_y in
          (* obsłuż kliknięcie w x,y *)
          handle_mouse_click (x,y) |> ignore
      | _ -> ()
      ;
      handle_events ()  (* sprawdź kolejne zdarzenie w kolejce *)
    end
  in

  handle_events ();

  draw renderer !state !my_game;
    
  if !running then 
    Lwt_unix.sleep 0.016 >>= fun () -> gui_loop renderer
  else Lwt.return_unit

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
      receiver ()
    | "UPDATE" :: fen :: _ -> 
      (* Lwt.async (fun () -> Lwt_io.printl (Printf.sprintf "updating fen %s" fen)); *)
      update_board fen;
      receiver ()
    | "INVALID" :: _ -> receiver ()
    | "HIGHLIGHT" :: to_highlight -> 
      (* Lwt.async (fun () -> Lwt_io.printl ("printing highlight"));
      Lwt.async (fun () -> Lwt_io.printl (List.fold_left (^) "" to_highlight));
      Lwt.async (fun () -> Lwt_io.printl (List.fold_left (fun acc x -> acc ^ (string_of_int x)) "" (List.map int_of_string to_highlight))); *)
      new_highlight (List.map int_of_string to_highlight);
      receiver ()
    | "WIN" :: c :: _ -> 
      let g = get_game () in
      let mess = if Logic.string_of_color g.my_color = c then "You won." else "You lost." in
      Lwt.async (fun () -> Lwt_io.printl mess);
      state := InLobby;
      my_game := None;
      receiver ()
    | "DRAW" :: _ -> 
      Lwt.async (fun () -> Lwt_io.printl "Game ended in a draw");
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

    (* opcjonalnie ustaw hint do skalowania *)
    Sdl.set_hint Sdl.Hint.render_scale_quality "linear" |> ignore;

    (* tu możesz narysować początkowe lobby *)

    (* uruchom Lwt pętle *)
    
    Lwt.join [
      gui_loop renderer;
      receiver ();
      sender ();
    ]

  (* cleanup SDL – nastąpi dopiero jak wszystkie pętle skończą *)



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
