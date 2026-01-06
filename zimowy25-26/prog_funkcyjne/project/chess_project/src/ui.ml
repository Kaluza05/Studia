(* open Js_of_ocaml
open Logic

let filename_of_figure (f : figure) : string = 
  let f_name = match f with
  | White, King   -> "w_king.png"
  | White, Queen  -> "w_queen.png"
  | White, Rook   -> "w_rook.png"
  | White, Bishop -> "w_bishop.png"
  | White, Knight -> "w_knight.png"
  | White, Pawn   -> "w_pawn.png"
  | Black, King   -> "b_king.png"
  | Black, Queen  -> "b_queen.png"
  | Black, Rook   -> "b_rook.png"
  | Black, Bishop -> "b_bishop.png"
  | Black, Knight -> "b_knight.png"
  | Black, Pawn   -> "b_pawn.png"
  in "piece_images/" ^ f_name
  

let sq = 80
let board_size = 80 * 8

let square_color i j =
  if (i + j) mod 2 = 0
  then "rgba(255, 255, 255, 1)"
  else "rgba(0, 0, 0, 1)"

let draw_board (ctx : Dom_html.canvasRenderingContext2D Js.t) =
  for i = 0 to 7 do
    for j = 0 to 7 do
      ctx##.fillStyle := Js.string (square_color i j);
      ctx##fillRect
        (float_of_int (i * sq))
        (float_of_int (j * sq))
        (float_of_int sq)
        (float_of_int sq)
    done
  done

let load_image src onload =
  let img = Dom_html.createImg Dom_html.document in
  img##.src := Js.string src;
  img##.onload := Dom_html.handler (fun _ -> onload img; Js._false);
  img

let canvas =
  Js_of_ocaml.Dom_html.getElementById "board"
  |> Option.some
  |> (Fun.flip Option.bind) (fun e -> 
    let a = Js_of_ocaml.Dom_html.CoerceTo.canvas e in
    a |> Js.Opt.to_option)
  |> function 
    | Some c -> c
    | None -> failwith "canvas not found"



let draw_piece ctx i j img =
  ctx##drawImage
    (img :> Dom_html.element Js.t)
    (float_of_int (i * sq))
    (float_of_int ((7 - j) * sq))
    (float_of_int sq)
    (float_of_int sq)

let draw_position ctx (board : position) =
  List.iteri
    (fun idx sq ->
      match sq with
      | None -> ()
      | Some fig ->
        let i,j = unflatten_pos idx in
        let filename = filename_of_figure fig in
        ignore
          (load_image filename (fun img ->
               draw_piece ctx i j img)))
    board

let main () =
  let canvas =
    Dom_html.getElementById_exn "board"
    |> Dom_html.CoerceTo.canvas
    |> Js.Opt.to_option
    |> Option.get
  in

  let ctx = canvas##getContext Dom_html._2d_ in

  draw_board ctx;

  (* przykład — tu podłączysz Logic.initial_position lub game_state.position *)
  let empty_board : position = List.init 64 (fun _ -> None) in

  draw_position ctx empty_board

let () = main () *)