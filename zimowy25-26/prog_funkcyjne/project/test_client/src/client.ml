open Graphics
open Logic
open Lwt.Syntax
open Lwt.Infix

open Image
open ImagePNG

let img =
  let png =  "knight.png" in
  png

let tile_size = 60

let width = 8 * tile_size
let height = 8 * tile_size


let join_button = (200, 200, 200, 80) (* x1,y1,x2,y2 *)
let leave_button = (200, 200, 200, 80) (* x1,y1,x2,y2 *)

(* let get_x1 (x1,_,_,_) = x1
let get_y1 (_,y1,_,_) = y1
let get_x2 (_,_,x2,_) = x2
let get_y2 (_,_,_,y2) = y2 *)

let inside (x,y) (x1,y1,w,h) =
  let x2, y2 = x1 + w, y1 + h in
  x >= x1 && x <= x2 && y >= y1 && y <= y2

let outside_window (x,y) = 
  x < 0 || y < 0 || x > width || y > height

let draw_lobby () =
  clear_graph ();
  set_color black;
  let x1,y1,x2,y2 = join_button in 
  draw_rect x1 y1 x2 y2;
  moveto (x1 + x2 / 2) (y1 + y2 / 2);
  draw_string "Join queue"

let draw_queue () =
  clear_graph ();
  set_color black;
  let x1,y1,x2,y2 = leave_button in 
  draw_rect x1 y1 x2 y2;
  moveto (x1 + x2 / 2) (y1 + y2 / 2);
  draw_string "Leave queue"




let draw_board () =
  clear_graph ();
  for x = 0 to 7 do
    for y = 0 to 7 do
      if (x + y) mod 2 = 0 then set_color white else set_color black;
      fill_rect (x*tile_size) (y*tile_size) tile_size tile_size
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
  my_color : Logic.color;
  turn : Logic.color}




let screen_pos_to_square (col : Logic.color) (x,y) = 
  let r,c = x / tile_size, y/tile_size in 
  match col with
  | White -> flatten_pos (7-c) r
  | Black -> flatten_pos c (7-r)


let get_square (col : Logic.color) (x,y) = screen_pos_to_square col (x,y) |> int_to_pos
(* 
(0,600) -> (0,0) -> 0 
(0,0) -> -> (7,0) -> 56
 *)

(* let send_move from to_sq = 
  failwith "a"

let handle_click_ingame sq state =
  match state.selected with
  | None ->
      if owns_piece state.board sq state.my_color && state.turn = state.my_color then
        { state with
          selected = Some sq;
          highlights = legal_moves state.board sq }
      else
        state

  | Some from ->
      if sq = from then
        { state with selected = None; highlights = [] }

      else if List.mem sq state.highlights then (
        send_move from sq;
        state (* czekasz na APPLY z serwera *)
      )
      else
        state  klik w złe pole: ignoruj *)

let my_game  = ref None

let select_new sel = 
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
    Lwt.async (fun () -> Lwt_io.printl ("new turn is : " ^ (match new_turn with White -> "White" | Black -> "Black")));
  my_game := Some {board = new_board; selected = None; to_highlight = []; my_color = g.my_color; turn = new_turn} 

(* separate functions because board is flipped for black *)
let square_pos_to_screen (col : color) (p : int) = 
  let r,c = unflatten_pos p in
  match col with
  | White -> tile_size * c, tile_size * (7-r)
  | Black -> tile_size * (7-c), tile_size * r


(* let draw_piece (color : Logic.color) (pos : int) (c,p : figure) = 
  let x,y = square_pos_to_screen color pos in
  x,y *)


let draw_square (color : Logic.color) pos f = 
  let fig = 
  match f with
  | None                 -> ()
  | Some (White, Pawn  ) -> failwith "wnfoiawnf"
  | Some (White, Rook  ) -> failwith "wnfoiawnf"
  | Some (White, Knight) -> failwith "wnfoiawnf"
  | Some (White, Bishop) -> failwith "wnfoiawnf"
  | Some (White, King  ) -> failwith "wnfoiawnf"
  | Some (White, Queen ) -> failwith "wnfoiawnf"
  | Some (Black, Pawn  ) -> failwith "wnfoiawnf"
  | Some (Black, Rook  ) -> failwith "wnfoiawnf"
  | Some (Black, Knight) -> failwith "wnfoiawnf"
  | Some (Black, Bishop) -> failwith "wnfoiawnf"
  | Some (Black, King  ) -> failwith "wnfoiawnf"
  | Some (Black, Queen ) -> failwith "wnfoiawnf"
  (* in draw_piece color pos fig *)in failwith "c"



let draw_board () =
  for x = 0 to 7 do
    for y = 0 to 7 do
      if (x + y) mod 2 = 0 then set_color white else set_color black;
      fill_rect (x*tile_size) (y*tile_size) tile_size tile_size
    done
  done


let draw_selected ?(color = green) (col : Logic.color) (p : int) = 
  let x,y = square_pos_to_screen col p in
  Lwt.async (fun () -> Lwt_io.printl (Printf.sprintf "pozycja: %d %d" x y));
  set_color color;
  fill_rect x y tile_size tile_size



let draw_figures g =
  clear_graph ();
  let col,selected,to_highlight = g.my_color, g.selected, g.to_highlight in
  draw_board ();
  match selected with
  | None -> ()
  | Some p -> p |> pos_to_int |> draw_selected ~color:blue col
  ;
  List.iter (draw_selected ~color:green col) to_highlight
  (* List.iteri (fun pos f -> draw_figure pos f) g.board *)
(* 
drab_board()
draw_selected ()
List.iter (draw_selected ~color:) to_highlight
narysuj plansze najpierw,
narysuj selected i highlighted
narysuj figury

*)


  (* match g.my_color with
  | White -> 
  | Black -> *)
(* get selected square *)

let draw state game_ui = 
  match state with
  | InLobby -> draw_lobby ()
  | InQueue -> draw_queue ()
  | InGame -> 
    match game_ui with 
    | None -> failwith "in game but no game to draw"
    | Some g -> 
      draw_figures g
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

let do_stuff input output = 
  let outgoing = Lwt_mvar.create_empty () in
  let state = ref InLobby in

  let rec gui_loop () =
    Lwt.catch (fun () ->

    (* Lwt.async (fun () -> Lwt_io.printl (string_of_state !state)); *)
    draw !state !my_game;
    (if button_down () then (
      let x,y = mouse_pos () in
      if outside_window (x,y) then gui_loop () else 
      match !state with
      | InLobby when inside (x,y) join_button ->
          state := InQueue;
          Lwt.async (fun () -> Lwt_io.write_line output "JOIN");
          Lwt.return_unit
      | InQueue when inside (x,y) leave_button -> 
          state := InLobby;
          Lwt.async (fun () -> Lwt_io.write_line output "LEAVE");
          Lwt.return_unit
      | InGame ->
        begin match !my_game with
        | None -> 
          Lwt.async (fun () -> Lwt_io.printl "this should be impossible case");
          Lwt.return_unit
        | Some g -> 
          Lwt.async (fun () -> 
            if g.turn = g.my_color 
            then  Lwt.return_unit
            else Lwt_io.printl "not my turn"
            );
          if g.turn = g.my_color then
            let square = get_square (g.my_color) (x,y) in
            Lwt.async (fun () -> Lwt_io.printl ("clicked square: " ^ square));
            Lwt.async (fun () -> Lwt_io.printl ("currently selected: " ^ show_selected g.selected));
            
            Lwt.async (fun () -> Lwt_io.printl ("current board:\n" ^ show_fake_board g.board));
            match g.selected with
            | None -> 
              (if is_my_color g.board g.my_color square then 
                select_new (Some(square));
                Lwt.async (fun () -> Lwt_io.write_line output (Printf.sprintf "SELECT %s" square));
                );
                Lwt.return_unit
              (* check if its our figure and select it *)
              (* make it the new selected *)
            | Some p ->
              begin match p with
              | _ when p = square -> 
                select_new None;
                new_highlight [];
                Lwt.return_unit
                (*deselect the square*)
              | _ when not (List.mem (Logic.pos_to_int square) g.to_highlight) -> Lwt.return_unit (*do nothing cuz click is in the wrong place*)
              | _ -> 
                select_new None; (*deselect after making a move*)
                new_highlight [];
                Lwt.async (fun () -> Lwt_io.printl  (Printf.sprintf "debug MOVE %s %s" p square));
                Lwt.async (fun () -> Lwt_io.write_line output (Printf.sprintf "MOVE %s %s" p square));
                Lwt.return_unit
              end
            else Lwt.return_unit
               (* "send it to the server and see if it's a correct move by server answer
                but we can do it ourselves now and server will double check us" 
               if p = square then make selected none*)
        end
      | _ -> 
        Lwt.async (fun () -> Lwt_io.printl (Printf.sprintf "pos: %d %d" x y));
        Lwt.async (fun () -> Lwt_io.printl (Printf.sprintf "some other case, game state: %s" (string_of_state !state)));
        Lwt.async (fun () -> Lwt_io.write_line output "some other case");
        Lwt.return_unit

      )
      else Lwt.return_unit)
    >>= fun () ->

    Lwt_unix.sleep 0.1
     >>= fun () -> 
    gui_loop ())

     (fun ex ->
      Lwt_io.printl ("EXCEPTION: " ^ Printexc.to_string ex)
    )
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
      Lwt.async (fun () -> Lwt_io.printl (Printf.sprintf "updating fen %s" fen));
      update_board fen;
      receiver ()
    | "INVALID" :: _ -> receiver ()
    | "HIGHLIGHT" :: to_highlight -> 
      Lwt.async (fun () -> Lwt_io.printl ("printing highlight"));
      Lwt.async (fun () -> Lwt_io.printl (List.fold_left (^) "" to_highlight));
      Lwt.async (fun () -> Lwt_io.printl (List.fold_left (fun acc x -> acc ^ (string_of_int x)) "" (List.map int_of_string to_highlight)));
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

  open_graph (Printf.sprintf " %dx%d" width height);
  set_window_title "Client";
  clear_graph ();
  draw_lobby ();

  Lwt.join [gui_loop (); receiver (); sender ()]



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







(* 
rozbić to co mam, co potrzeba:
rysowanie planszy,
obsluga kliknięć na planszy,
odbieranie wiadomosci z serwera

*)

(* open Tsdl
open Tsdl_image
open Result

(* rozmiar planszy *)
let square_size = 64
let board_size = 8

(* ładowanie obrazka z PNG *)
let load_texture renderer file =
  match Img.load_texture renderer file with
  | Ok tex -> tex
  | Error (`Msg e) -> failwith e

(* rysowanie planszy *)
let draw_board renderer white_side =
  for y = 0 to 7 do
    for x = 0 to 7 do
      let color =
        if (x + y) mod 2 = 0 then (255,255,255) (* białe *)
        else (139,69,19) (* brązowe *)
      in
      let r = Sdl.Rect.create ~x:(x*square_size) ~y:(y*square_size)
                ~w:square_size ~h:square_size in
      Sdl.set_render_draw_color renderer (fst3 color) (snd3 color) (trd3 color) 255;
      Sdl.render_fill_rect renderer (Some r) |> ignore
    done
  done

(* pomocnicze *)
let fst3 (x,_,_) = x
let snd3 (_,y,_) = y
let trd3 (_,_,z) = z

(* rysowanie figur *)
let draw_pieces renderer pieces white_side =
  for y = 0 to 7 do
    for x = 0 to 7 do
      let piece = pieces.(y).(x) in
      match piece with
      | None -> ()
      | Some tex ->
        let draw_x, draw_y =
          if white_side then (x*square_size, y*square_size)
          else ((7-x)*square_size, (7-y)*square_size)
        in
        let r = Sdl.Rect.create ~x:draw_x ~y:draw_y ~w:square_size ~h:square_size in
        Sdl.render_copy renderer tex None (Some r) |> ignore
    done
  done

(* przykładowa plansza z pionkami *)
let init_pieces renderer =
  let tex_white_pawn = load_texture renderer "white_pawn.png" in
  let tex_black_pawn = load_texture renderer "black_pawn.png" in
  Array.init 8 (fun y ->
    Array.init 8 (fun x ->
      if y = 1 then Some tex_white_pawn
      else if y = 6 then Some tex_black_pawn
      else None
    )
  )

let () =
  Sdl.init Sdl.Init.video |> R.ok_or_failwith;
  Img.init Img.Init.png |> ignore;

  let window = Sdl.create_window ~w:(square_size*8) ~h:(square_size*8)
                "Szachy TSdl" Sdl.Window.windowed |> R.get_ok in
  let renderer = Sdl.create_renderer window |> R.get_ok in

  (* wybór gracza: true = białe u dołu, false = czarne u dołu *)
  let white_side = true in

  let pieces = init_pieces renderer in

  (* główna pętla renderowania *)
  let rec loop () =
    Sdl.set_render_draw_color renderer 0 0 0 255;
    Sdl.render_clear renderer |> ignore;

    draw_board renderer white_side;
    draw_pieces renderer pieces white_side;

    Sdl.render_present renderer;

    (* prosta obsługa zdarzeń: zamknięcie okna *)
    let e = Sdl.Event.create () in
    if Sdl.poll_event (Some e) then
      match Sdl.Event.(get e typ) with
      | x when x = Sdl.Event.quit -> ()
      | _ -> loop ()
    else loop ()
  in

  loop ();

  Sdl.destroy_renderer renderer;
  Sdl.destroy_window window;
  Img.quit ();
  Sdl.quit () *)


  
  (* 
  najpierw zrobię wszystko od strony serwera zakładając, że po stronie klienta juz to istnieje

  mozliwe dodatki poznije, rozne typy gry, fisher random, crazyhouse, fog of war - zmiana w logic

  
  co moze za request wyslac klient?

klient musi tylko wiedzieć co wyświetlać i czyj jest ruch
a nawet nie musi tego wiedzzieć czy jego ruch, to można po stronie serwera trzymać
  
klient klika na jakieś pole
jesli jego tura podswietlaja sie odpowiednie pola

jesli nic nie wybrane i to nasza figura - wyslij do serwera select <pole>
jesli wybrane to samo pole - nie pisz do serwera po prostu odznacz
jesli wybrane pole ktore jest wsrod pol do podswietlenia - wyslij do serwera move <pole1> <pole2> i odznacz to pole i wyczyść podswietlenia

zna swoj kolor wiec moze z planszy zgarnac swoje figury i zobaczyc czy selected jest jego, to mozna po tej stronie
tak samo bedzie mial ktore sa do podswietlenia czyli bedzie mogl sprawdzic czy jego pole jest tutaj


  *)