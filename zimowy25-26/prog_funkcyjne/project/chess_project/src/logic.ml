type color = White | Black
type piece = Pawn | Rook | Knight | Bishop | King | Queen
type figure = color * piece
type square = figure option
type position = square list
type castle_rights = 
  { white_kingside  : bool;
    white_queenside : bool;
    black_kingside  : bool;
    black_queenside : bool }
type board = position * color * castle_rights  * ((color * int) option)

(*board, turn, white castle queen/king side, black castle, enpassant column*)
type repetition_tracker = (int64,int) Hashtbl.t

type game_state = {
  board : board;
  repetitions : repetition_tracker;
  no_takes_counter : int;
  coding : string list;
}
(* remove the coding in chessbot so it runs faster *)


type game = 
  | Playing of game_state
  | Win of color
  | Draw

type move = string * string
(*
kodowanie na dwa sposoby,
doczepianie kodowania pozycji do ruchu (monada)


kodowanie tak jak się zapisuje partie H8, #F5
*)

let piece_to_int (p : piece) : int = 
  match p with
  | King   -> 5
  | Queen  -> 4
  | Rook   -> 3
  | Bishop -> 2
  | Knight -> 1
  | Pawn   -> 0 
let comp_piece (p1 : piece) (p2 : piece) = Int.compare (piece_to_int p1)  (piece_to_int p2)


let list_list_map (f : 'a -> 'b) (xs : 'a list list) : 'b list list = List.map (fun r -> List.map f r) xs

(** real modulo operator *)
let rmod a m =
  let r = a mod m in
  if r < 0 then r + m else r

let filter_indices xs p =
  let rec it i acc = function
    | [] -> List.rev acc
    | x :: xs ->
        let acc =
          if p x then i :: acc else acc
        in
        it (i+1) acc xs
  in
  it 0 [] xs

let string_of_color (c : color) : string = 
  match c with
  | White -> "White"
  | Black -> "Black"

(** negation of color *)
let (!!) (c : color) = 
  match c with
  | White -> Black
  | Black -> White

let get_board (b,_,_,_ : board) = b
let get_turn (g : game) =
  match g with
  | Playing {board = _,t,_,_;_} -> t
  | Draw | Win _ -> failwith "game already finished, cant get turn"

let init_board : board = 
  let empty_row = List.init 8 (fun _ -> None) in 
  let pawn_row  = List.init 8 (fun _ -> Pawn) in
  let figure_row = [Rook;Knight;Bishop;Queen;King;Bishop;Knight;Rook] in
  let add_color (c : color) (row : piece list) = List.map (fun x -> Some(c,x)) row          in
  let b = List.flatten [
    add_color Black figure_row; 
    add_color Black pawn_row; 
    empty_row;
    empty_row;
    empty_row;
    empty_row;
    add_color White pawn_row; 
    add_color White figure_row] in
  b,White,({white_kingside = true;white_queenside = true;
            black_kingside = true;black_queenside = true}), None


let game_to_board (g : game) : board = 
  match g with
  | Playing {board; _} -> board
  | _ -> failwith "game already finished"

let init_game () : game =  Playing({board = init_board; repetitions = Hashtbl.create 128; no_takes_counter = 0; coding = []})

(* board is represented as a flat list of len 64, turn it into a 8 * 8 list of rows *)
(*
pos 0 is A8
pos 63 is H1
*)

let cart_prod (xs : 'a list) (ys : 'b list) : ('a * 'b) list = 
  List.concat_map (fun x -> List.map (fun y -> (x,y)) ys) xs

let (><) = cart_prod

let insert_at_n (xs : 'a list) (x : 'a) (n : int) = 
  let rec it xs n acc = 
    match xs with
    | [] -> failwith "index out of range"
    | y :: xs -> if n = 0 then (List.rev acc) @ x :: xs else  it xs (n-1) (y :: acc)
  in it xs n []

let take_n (xs : 'a list) (n : int) : 'a list * 'a list = 
  let rec it xs n acc = if n = 0 then List.rev acc, xs else 
    match xs with
    | [] -> failwith "index out of range"
    | x :: xs -> it xs (n-1) (x :: acc)
  in it xs n []

(* n has to divide len(xs) *)
(** unflattens list of size n * m into a list of lists of size n eq. unflatten [[1,2,3,4,5,6]] 2 = [[[1,2],[3,4],[5,6]]]*)
let rec unflatten (n : int) (xs : 'a list) : 'a list list = 
  if List.length xs mod n <> 0 then failwith "n doesn't divide length of list xs" else
  match xs with 
  | [] -> []
  | _  -> let h,t = take_n xs n in h :: unflatten n t

let unflatten_board (b : figure option list) : figure option list list = unflatten 8 b

(**returns a pair row * col for a position on a flat board
row = pos / 8, col = pos mod 8*)
let unflatten_pos (pos : int) = pos / 8, pos mod 8
let flatten_pos (row : int) (col : int) : int = row * 8 + col



let pos_in_board (r,c : int * int) = r >= 0 && c >= 0 && r < 8 && c < 8


let board_nth_2d (b : position) (row : int) (col : int) : figure option = List.nth b (flatten_pos row col)

(*
___ ___ ___ ___ ___ ___ ___ ___
___ ___ ___ BQ  ___ ___ ___ ___
___ ___ ___ ___ ___ ___ ___ ___
___ ___ ___ BKn ___ ___ ___ ___
___ ___ ___ ___ ___ ___ ___ ___
___ ___ ___ ___ ___ ___ ___ ___
___ ___ ___ WR  ___ ___ WKi ___
___ ___ ___ ___ ___ ___ ___ ___ 
*)



(* returns whether color t is under atack now *)
(* to change , some recursion with possible moves needed i think*)

(** returns position of a king of a given color  *)
(* let king_pos (board : board_only) (t : color) = 
  List.find_index (fun f ->  match f with
      | Some(t', King) when t' = t -> true
      | _ -> false) board
  |> Option.get *)


let king_pos (board : position) (t : color) = 
  match List.find_index (fun f ->  match f with
      | Some(t', King) when t' = t -> true
      | _ -> false) board
  with
  | Some(v) -> v
  | None -> failwith "error at getting king pos"


let add_row (b,_,_,_ : board ) (pos : int) : (int * int) list =
  let row,col = unflatten_pos pos in
  let b' = unflatten_board b in
  let curr_row = List.nth b' row in
  let c = (List.nth b pos) |> Option.get |> fst in (*overrides the c from turn on purpose for now*)

  let rec it col acc inc = 
    if col < 0 || col >= 8 then acc (*went out of range*) else
    match (List.nth curr_row col) with
    | None -> it (col + inc) ((row,col) :: acc) inc
    | Some (c',_) -> if !!c = c' then (row,col) :: acc else acc   (*still adds but only if the color is different*)
  in (it (col + 1) [] 1) @ (it (col - 1) [] (-1))

let add_col (b,_,_,_ : board ) (pos : int) : (int * int) list =
  let row,col = unflatten_pos pos in
  let b' = unflatten_board b in
  let curr_col = List.map (fun r -> List.nth r col) b' in
  let c = (List.nth b pos) |> Option.get |> fst in (*overrides the c from turn on purpose for now*)

  let rec it row acc inc = 
    if row < 0 || row >= 8 then acc (*went out of range*) else
    match (List.nth curr_col row) with
    | None -> it (row + inc) ((row,col) :: acc) inc
    | Some (c',_) -> if !!c = c' then (row,col) :: acc else acc   (*still adds but only if the color is different*)
  in (it (row + 1) [] 1) @ (it (row - 1) [] (-1))

(* diagonal going up like A1 to H8 *)
(** checks if position is in the board 8x8 *)

(* cos trzeba bedzie zme3inic w tych temp_diag ale to jutro  *)
let add_diag1 (b,_,_,_ : board ) (pos : int) : (int * int) list =
  let row,col = unflatten_pos pos in
  let temp_diag = List.init 16 (fun i -> row - i + 8,col + i - 8) in
  let curr_diag = List.filter pos_in_board temp_diag in

  let left_side,right_side = List.rev (List.filter (fun (r,_) -> r > row) curr_diag), List.filter (fun (r,_) -> r < row) curr_diag in
  let c = (List.nth b pos) |> Option.get |> fst in (*overrides the c from turn on purpose for now*)

  let rec it pos_list acc = 
    match pos_list with
    | [] -> acc
    | (row,col) :: pos_list -> 
      begin match board_nth_2d b row col with
        | None -> it pos_list ((row,col) :: acc)
        | Some (c',_) -> if !!c = c' then (row,col) :: acc else acc
      end 
  in it left_side [] @ it right_side []

let add_diag2 (b,_,_,_ : board ) (pos : int) : (int * int) list =
  let row,col = unflatten_pos pos in
  let temp_diag = List.init 16 (fun i -> row + i - 8,col + i - 8) in
  let curr_diag = List.filter pos_in_board temp_diag in
  let left_side,right_side = List.rev (List.filter (fun (r,_) -> r < row) curr_diag), List.filter (fun (r,_) -> r > row) curr_diag in
  let c = (List.nth b pos) |> Option.get |> fst in (*overrides the c from turn on purpose for now*)

  let rec it pos_list acc = 
    match pos_list with
    | [] -> acc
    | (row,col) :: pos_list -> 
      begin match board_nth_2d b row col with
        | None -> it pos_list ((row,col) :: acc)
        | Some (c',_) -> if !!c = c' then (row,col) :: acc else acc
      end 
  in it left_side [] @ it right_side []


(*cart product (2,-2)x(1,-1), (1,-1)x(2,-2) *)
let add_knight (b,_,_,_ : board) (pos : int) : (int * int) list = 
  let t = (List.nth b pos) |> Option.get |> fst in
  let row,col = unflatten_pos pos in 
  let temp_moves = ([2;-2] >< [1;-1]) @ ([1;-1] >< [2;-2]) |> List.map (fun (i,j) -> row + i, col + j)  in 
  let moves_in = List.filter pos_in_board temp_moves in
  let moves = moves_in |> List.filter (fun (r,c) -> 
    match List.nth b (flatten_pos r c) with
    | None -> true
    | Some (c',_) -> !!c' = t ) 
  in
  moves

let pawn_attacks (b,_,_,_) (pos : int) (color : color) = 
  let inc = match color with
  | White -> -1
  | Black ->  1
  in
  let row,col = unflatten_pos pos in
    [(row + inc,col+1);(row + inc, col-1)] 
    |> List.filter pos_in_board 
    |> List.filter (fun (r,c) -> match List.nth b (flatten_pos r c) with
      | Some(color',_) when !!color = color' -> true
      | _ -> false) 

let add_pawn (b,t',castle ,e: board) (pos : int) (color : color) = 
  let inc = match color with
  | White -> -1
  | Black ->  1
  in
  let row,col = unflatten_pos pos in
  (* forward, if on the second file can move by two (can do enpassant then but add that later), when at last file do a promotion *)
  let enpassant = if row = rmod (4 * inc) 7 then 
    begin match e with
    | Some(c', p) when c' = !!color -> 
      let _, col' = unflatten_pos p in
      if col' = col + 1 then [(row + inc,col+1)] else 
      if col' = col - 1 then [(row + inc, col - 1)] 
      else []
    | _ -> [] 
    end 
    else [] 
  in
  let takes = pawn_attacks (b,t',castle,e) pos color
  in
  let moves_forward = 
    if Option.is_none (List.nth b (flatten_pos (row + inc) col)) then (*row-1 should always be >=0 because we cant have pawn on last file*)
      if row = rmod inc 7 && Option.is_none (List.nth b (flatten_pos (row + 2 * inc) col)) then 
        [(row + 2 * inc,col);(row + inc,col)] 
      else [(row + inc,col)]
    else
      []
  in
  takes @ moves_forward @ enpassant




let king_attacks (b : board) (pos : int) : (int * int) list = 
  let t = (List.nth (get_board b) pos) |> Option.get |> fst in
  let row,col = unflatten_pos pos in
  let temp_moves = ([1;-1] >< [1;-1]) @ ([1;-1] >< [0]) @ ([0] >< [1;-1]) |> List.map (fun (i,j) -> row + i, col + j) in
  let moves_in = List.filter pos_in_board temp_moves in
  let moves = moves_in |> List.filter (fun (r,c) -> 
    match List.nth  (get_board b) (flatten_pos r c) with
    | None -> true
    | Some (c',_) -> !!c' = t)
  in 
  moves



let possible_for_wpawn (b : board) (pos : int) = add_pawn b pos White
let possible_for_bpawn (b : board) (pos : int) = add_pawn b pos Black 
let possible_for_rook   (b : board)  (pos : int) = 
(* get all positions  *)
(* get figures on the same row, col and add everything between *)
  add_row b pos @ add_col b pos
  (* z tego odjąć wszystkie *)

let possible_for_knight (b : board)  (pos : int) = add_knight b pos
let possible_for_bishop (b : board)  (pos : int) = 
  add_diag1 b pos @ add_diag2 b pos 
let possible_for_queen  (b : board)  (pos : int) = 
  add_row b pos @ add_col b pos @ add_diag1 b pos @ add_diag2 b pos 




module ExceptionMonad : sig

  val return : board -> board option
  val bind : board option -> (board -> board option) -> board option
  val fail : board option

  val catch : board option -> (unit -> board option) -> board option

end = struct
  let return b = Some b
  let bind m f =
    match m with
    | None   -> None
    | Some x -> f x

  let fail = None

  let catch m f =
    match m with
    | None   -> f ()
    | Some x -> Some x

end
let (let*) = ExceptionMonad.catch


(**  returns a list of squares attacked by the figure at pos  *)
let attacked_positions (b : board) (pos : int) : (int * int) list = 
  match List.nth (get_board b) pos with
  | None -> failwith "can't chose empty square"
  | Some fig ->
    begin match fig with
    | c, Pawn     -> pawn_attacks        b pos c
    | _ , Rook    -> possible_for_rook   b pos
    | _, Knight   -> possible_for_knight b pos
    | _, Bishop   -> possible_for_bishop b pos
    | _, King     -> king_attacks        b pos
    | _, Queen    -> possible_for_queen  b pos
    end

(** returns list of positions of all pieces of a given color*)
let all_pieces (b : board) (t : color) : int list = 
  filter_indices (get_board b) 
  (fun f -> match f with 
  | Some(t',_) when t = t' -> true 
  | _ -> false) 

(** checks is square at pos is attacked by given color*)
let is_attacked (b : board) (pos : int) (t : color) = 
  let all_pieces = all_pieces b t in
  let attacked = List.concat_map (attacked_positions b) all_pieces in
  List.mem (unflatten_pos pos) attacked
  


(** checks if color t is under check  *)
let is_check (b : board) (t : color) = 
  let k_pos = king_pos (get_board b) t in
  is_attacked b k_pos !!t



let can_castle_kingside (brd : board) (col : color) : bool =
  let b,t,castle_rights,_ = brd in 
  (* hardcoded positions where the figures are *)
  match col with
  | White -> List.for_all (fun i -> not (is_attacked brd i !!t)) [60;61;62] 
    && castle_rights.white_kingside && 
    begin match List.nth b 60 , List.nth b 61, List.nth b 62, List.nth b 63 with
    | Some(White,King), None,None,Some(White,Rook) -> true
    | _ -> false
    end
  | Black -> List.for_all (fun i -> not (is_attacked brd i !!t)) [4;5;6]
    && castle_rights.black_kingside && 
    begin match List.nth b 4, List.nth b 5, List.nth b 6, List.nth b 7 with
    | Some(Black,King), None,None,Some(Black,Rook) -> true
    | _ -> false
    end
let can_castle_queenside (brd : board) (col : color) : bool =
  let b,t,castle_rights,_ = brd in
  match col with
  | White -> List.for_all (fun i -> not (is_attacked brd i !!t)) [58;59;60]
    && castle_rights.white_queenside && 
    begin match List.nth b 56 , List.nth b 57, List.nth b 58, List.nth b 59, List.nth b 60 with
    | Some(White,Rook), None,None,None,Some(White,King) -> true
    | _ -> false
    end
  | Black -> List.for_all (fun i -> not (is_attacked brd i !!t)) [2;3;4]
    && castle_rights.black_queenside &&
    begin match List.nth b 0, List.nth b 1, List.nth b 2, List.nth b 3, List.nth b 4 with
    | Some(Black,Rook), None,None,None,Some(Black,King) -> true
    | _ -> false
    end

let add_castle_kingside (b : board) (col : color) = 
  if can_castle_kingside b col
    then match col with
    | White -> [(7,6)]
    | Black -> [(0,6)]
  else []
let add_castle_queenside (b : board) (col : color) =
  if can_castle_queenside b col
    then match col with
    | White -> [(7,2)]
    | Black -> [(0,2)]
  else []
  

let add_castle (b : board) (col : color) : (int * int) list = add_castle_kingside b col @ add_castle_queenside b col

let add_king (b : board) (pos : int) = 
  let t = (List.nth (get_board b) pos) |> Option.get |> fst in
  let moves = king_attacks b pos in
  let moves_castle = add_castle b t @ moves in
  moves_castle

let possible_for_king   (b : board)  (pos : int) = add_king b pos


let int_of_letter (c : char) : int = 
  match Char.lowercase_ascii c with
  | 'a' -> 0
  | 'b' -> 1
  | 'c' -> 2
  | 'd' -> 3
  | 'e' -> 4
  | 'f' -> 5
  | 'g' -> 6
  | 'h' -> 7
  | _ -> failwith "wrong char given"
  
let letter_of_int (i : int) : char = 
  match i with
  | 0 -> 'A'
  | 1 -> 'B'
  | 2 -> 'C'
  | 3 -> 'D'
  | 4 -> 'E'
  | 5 -> 'F'
  | 6 -> 'G'
  | 7 -> 'H'
  | _ -> failwith "wrong int given"
let int_of_num (c : char) : int = 
  match c with
  | '1' -> 1
  | '2' -> 2
  | '3' -> 3
  | '4' -> 4
  | '5' -> 5
  | '6' -> 6
  | '7' -> 7
  | '8' -> 8
  | _   -> failwith "wrong char"

let pos_to_int (pos : string) = 
  if String.length pos <> 2 then failwith "wrong position" else
  let l, n = String.get pos 0 |> int_of_letter, String.get pos 1 |> int_of_num in
  if n < 1 || n > 8 then failwith "numer in the wrong range" else
  flatten_pos (8 - n) l

let int_to_pos (p : int) = 
  if p < 0 || p >= 64 then failwith "wrong pos" else
  let r,c = unflatten_pos p in
  String.make 1 (letter_of_int c) ^ string_of_int (8 - r)
let try_double_pawn_move (b,t,c,_ : board) (curr_pos : int) (go_to : int) = 
  match List.nth b curr_pos with
  | Some(_,Pawn) -> 
    let row,col = unflatten_pos curr_pos 
    and row',col' = unflatten_pos go_to 
    in if abs (row - row') = 2 then 
    let b = insert_at_n b None curr_pos in 
    let b = insert_at_n b (Some(t,Pawn)) go_to in
    let e = if 
      (col' <> 7 && List.nth b (go_to + 1) = Some(!!t,Pawn)) 
      || 
      (col' <> 0 && List.nth b (go_to - 1) = Some(!!t, Pawn)) then Some(t,col) else None in
    let new_board = b,!!t, c, e in Some new_board 
  else None
  | _ -> None
let try_enpassant (b,t,c,_ : board) (curr_pos : int) (go_to : int) = 
  (* we already know we can do enpassant since it's in the correct moves *)
  let inc = match t with
  | White -> -1
  | Black ->  1
  in
  match List.nth b curr_pos, List.nth b go_to with
  | Some(_,Pawn), None -> 
    let _,col = unflatten_pos curr_pos 
    and _',col' = unflatten_pos go_to 
    in if abs (col - col') = 1 
      then 
      let b = insert_at_n b None curr_pos in 
      let b = insert_at_n b (Some(t,Pawn)) go_to in
      let b = insert_at_n b None (go_to - inc * 8) in
      let new_board = b,!!t, c, None in Some new_board 
    else None
  | _ -> None
let try_castle (b,t,castle_rights,_ : board) (curr_pos : int) (go_to : int) = 
  match List.nth b curr_pos with
  | Some(_,King) -> 
    let _,col   = unflatten_pos curr_pos
    and _',col' = unflatten_pos go_to
    in if abs(col' - col) = 2 then  
      let b = insert_at_n b (Some(t,King)) go_to in 
      let rook_go = if col' = 6 then curr_pos else curr_pos - 1 in
      let rook_pos = if col' = 6 then curr_pos + 3 else curr_pos - 4 in
      let b = insert_at_n b (Some(t,Rook)) rook_go in
      let b = insert_at_n b None rook_pos in
      let castle_rights = match t with
      | White -> {
        white_kingside = false; 
        white_queenside = false; 
        black_kingside = castle_rights.black_kingside; 
        black_queenside = castle_rights.black_queenside}
      | Black -> {
        white_kingside = castle_rights.white_kingside; 
        white_queenside = castle_rights.white_queenside; 
        black_kingside = false;
        black_queenside = false}
      in
      let new_board = b,!!t, castle_rights, None in Some new_board

    else None
  | _ -> None
let try_promotion (b,t,c,_ : board) (curr_pos : int) (go_to : int) (promo : piece) = 
  let inc = match t with
  | White -> -1
  | Black ->  1
  in
  let row,_ = unflatten_pos curr_pos in if row = rmod (-inc) 7 then 
  match List.nth b curr_pos with
  | Some(_,Pawn) ->
    let b = insert_at_n b None curr_pos in 
    let b = insert_at_n b (Some(t,promo)) go_to in
    let new_board = b, !!t, c,None in Some new_board
  | _ -> None
  else None

let update_regular_move (b,t,castle_rights,_ : board) (curr_pos : int) (go_to : int) = 
  match List.nth b curr_pos with
  | None -> failwith "not possible case"
  | Some(_,f) ->
    let castle_rights = match t,f with
    | White, King -> {
      white_kingside = false;
      white_queenside = false; 
      black_kingside = castle_rights.black_kingside;
      black_queenside = castle_rights.black_queenside}
    | Black, King -> {
      white_kingside = castle_rights.white_kingside; 
      white_queenside = castle_rights.white_queenside; 
      black_kingside = false;
      black_queenside = false}
    | White, Rook -> 
      if curr_pos = 56 then 
      {white_kingside = castle_rights.white_kingside;
      white_queenside = false; 
      black_kingside = castle_rights.black_kingside;
      black_queenside = castle_rights.black_queenside}
      else if curr_pos = 63 then 
      {white_kingside = false;
      white_queenside = castle_rights.white_queenside; 
      black_kingside = castle_rights.black_kingside;
      black_queenside = castle_rights.black_queenside} 
      else castle_rights
    | Black, Rook -> 
      if curr_pos = 0 then 
      {white_kingside = castle_rights.white_kingside;
      white_queenside = castle_rights.white_queenside; 
      black_kingside = castle_rights.black_kingside;
      black_queenside = false} 
      else if curr_pos = 7 then
      {white_kingside = castle_rights.white_kingside;
      white_queenside = castle_rights.white_queenside; 
      black_kingside = false;
      black_queenside = castle_rights.black_queenside} 
      else castle_rights(*not a castle so we cant castle no more*)
    | _ -> castle_rights
    in
    let b = insert_at_n b None curr_pos in 
    let b = insert_at_n b (Some(t,f)) go_to in
    let new_board = b,!!t,castle_rights,None in Some new_board


let string_to_color (s : string) : color = 
  if s = "white" then White else
  if s = "black" then Black else
  failwith "wrong color passed"

let color_to_string (c : color) : string = 
  match c with
  | White -> "W"
  | Black -> "B"

let piece_to_string (p : piece) : string = 
  match p with
  | Pawn   -> "P"
  | Rook   -> "R"
  | Knight -> "N"
  | Bishop -> "B"
  | Queen  -> "Q"
  | King   -> "K"
let fig_to_string (f : figure option) : string = 
  match f with
  | None -> "___"
  | Some (c,p) -> color_to_string c ^ piece_to_string p ^ " "

let print_list_list (to_str : 'a -> string) (xss : 'a list list) : unit =
  let print_list xs =
    xs
    |> List.map to_str
    |> String.concat " "
    |> print_endline
  in
  List.iter print_list xss
  

let  possible_moves_helper (board : board) (pos : int) : (int * int) list = 
  let moves = match List.nth (get_board board) pos with 
  | None -> failwith "can't chose empty square"
  | Some fig ->
    begin match fig with
    | White, Pawn -> possible_for_wpawn  board pos
    | Black, Pawn -> possible_for_bpawn  board pos
    | _ , Rook    -> possible_for_rook   board pos
    | _, Knight   -> possible_for_knight board pos
    | _, Bishop   -> possible_for_bishop board pos
    | _, King     -> possible_for_king   board pos
    | _, Queen    -> possible_for_queen  board pos
    (*filtrowac jesli ruch mialby spowodowac szach na naszym królu*)
    end
  in moves

let is_valid (b,t,castle,e : board) (curr_pos : int) (go_to : int) = 
  (match List.nth b curr_pos with
  | Some(c,_) when c = t -> true
  | _ -> false)
  && List.mem (unflatten_pos go_to) (possible_moves_helper (b,t,castle,e) curr_pos) 


(* jesli robimy ruch z possible_moves to nie chcemy zeby wywalilo jesli jest niepoprawny tylko nie patrzylo na ten przypadek
wiec robimy raise Check after move i wylapujemy w possible moves
jesli odpalilismy z poziomu ruchu i ruch jest zły  *)
exception CheckAfterMove of string
exception WrongColor     of string
exception EmptySquare    of string
exception InvalidMove    of string

let move_helper1 ?(from_possible_moves = false) ?(promo = Queen) (brd : board) (curr_pos : int) (go_to : int)  =
  (if is_valid brd curr_pos go_to |> not then 
  if not from_possible_moves then raise (InvalidMove(int_to_pos go_to)) else raise (CheckAfterMove("s"))
  else
  let* () =  try_double_pawn_move brd curr_pos go_to in
  let* () =  try_enpassant brd curr_pos go_to in
  let* () =  try_castle brd curr_pos go_to    in
  let* () =  try_promotion brd curr_pos go_to promo in
  let* () =  update_regular_move brd curr_pos go_to in
  ExceptionMonad.fail)
  |> Option.get


let possible_moves (board : board) (pos : int) : (int * int) list = 
  let _,t,_,_ = board in 
  let moves = possible_moves_helper board pos 
    in
    List.filter (fun (r,c) -> 
      try 
      let after_move = move_helper1 ~from_possible_moves:true board pos (flatten_pos r c) in 
      is_check after_move t |> not
      with
      | CheckAfterMove(_) -> false
      | e -> raise e 
      ) 
      moves



(**returns every square that can be visited in the current turn*)
let  all_moves (b : board) = 
  let _,t,_,_ = b in 
  let all_pieces = filter_indices (get_board b) 
  (fun f -> match f with 
  | Some(t',_) when t = t' -> true 
  | _ -> false) 
  in
  List.concat_map (fun pos -> possible_moves b pos) all_pieces

let is_terminal (brd : board) = all_moves brd = []

let is_checkmate (brd : board) = let b,t,_,_ = brd in is_attacked brd (king_pos b t) !!t && is_terminal brd

let zobrist _ = Int64.zero (*trzeba zaimplementować counter*)
let is_pawn_move (b,_,_,_ : board) (curr_pos : int) (_ : int) = 
  match List.nth b curr_pos with
  | Some(_,Pawn) -> true
  | _ -> false

let is_capture (b,_,_,_ : board) (_ : int) (go_to : int) = List.nth b go_to |> Option.is_some

let add_board (repetitions : repetition_tracker) (board : board) : unit = 
  let hashed_board = zobrist board in
  let count = match Hashtbl.find_opt repetitions hashed_board with
  | Some(_) -> 1 (*should be n + 1 but i dont have hashing finished*)
  | None -> 1
  in
  Hashtbl.replace repetitions hashed_board count


let is_draw ({board;repetitions;no_takes_counter;_} : game_state) : bool = 
  let hashed_board = zobrist board in
  let b,t,_,_ = board in 
  let is_stalemate  = not (is_attacked board (king_pos b t) !!t) && is_terminal board in
  let is_repeating  = 
    match Hashtbl.find_opt repetitions hashed_board with
    | Some(c) -> c = 5
    | None -> false in
  let is_50_move = no_takes_counter = 100 in
  let impossible_checkmate = 
    let sort_pieces (ps : int list) = 
      ps 
      |> List.map (fun i -> List.nth b i |> Option.get |> snd) 
      |> List.sort comp_piece
    in
    match sort_pieces (all_pieces board White), sort_pieces (all_pieces board Black) with
    | [King], [King] -> true
    | [King;Knight], [King] -> true
    | [King], [King;Knight] -> true
    | [King;Bishop],[King] -> true
    | [King], [King; Bishop] -> true
    | _ -> false
  in 
  is_stalemate || is_repeating || is_50_move || impossible_checkmate


(* promocje mozna zamienic zeby zwracalo funckje do ktorej przekaze sie fugure zeby moze lepiej wygladalo 
grajac w utopie *)
let code_board (b,_,_,_ : board) (curr_pos : int) (_ : int) =
  let figure = List.nth b curr_pos |> Option.get |> snd |> piece_to_string in figure
(* potrzeba:
is check
is_checkmate
is_capture
is_ambigous 
is_promotion*)

let move_helper2 ({board;repetitions; no_takes_counter;coding} : game_state) (curr_pos : int) (go_to : int) (promo : piece) : game = 
  let no_takes_counter = if is_pawn_move board curr_pos go_to || is_capture board curr_pos go_to then 0 else no_takes_counter + 1 in
  let coding = code_board board curr_pos go_to :: coding in
  let board = move_helper1 board curr_pos go_to ~promo in
  add_board repetitions board;
  let game_state = {board;repetitions;no_takes_counter;coding} in
  let _,t,_,_ = board in 
  if is_check board (!!t) then failwith "illegal move" else
  if is_checkmate board then Win !!t else
  if is_draw game_state then Draw else
  (* update counters *)
  
  Playing game_state

let move_int ?(promo = Queen) (curr_pos : int) (go_to : int) (g : game) : game =
  match g with
  | Playing game_state -> 
    move_helper2 game_state curr_pos go_to promo
  | Win c -> failwith ("game finished with player" ^ string_of_color c ^ "winning")
  | Draw -> failwith "game ended with a draw, can't move int"
  
let move ?(promo = Queen) (curr_pos : string) (go_to : string) (g : game) : game =
  let curr_pos, go_to = pos_to_int curr_pos, pos_to_int go_to in
  move_int curr_pos go_to g ~promo:promo


let resign (g : game) : game = 
  match g with
  | Playing ({board = _,t,_,_; _}) -> Win !!t
  | _ -> failwith "game already finished"

let get_winner (g : game) : color = 
  match g with
  | Win c -> c
  | Draw -> failwith "game ended with a draw, can-t get winner"
  | Playing _ -> failwith "game still in play"

(**returns a list of moves that can be done in a current turn*)
let get_all_moves (g : game) : (int * int) list = 
  let b = game_to_board g in
  let _,t,_,_ = b in 
  let pieces = all_pieces b t in
  pieces
  |> List.concat_map (fun p -> 
    let moves = possible_moves b p in
    moves |> List.map (fun (r,c) -> p,(flatten_pos r c)) ) 

let move_board_int ?(promo = Queen) (curr_pos : int) (go_to : int) (b : board) = 
  let wrong_game_state = {board = b;repetitions = Hashtbl.create 1; no_takes_counter = 0; coding = []} in
  move_helper2 wrong_game_state curr_pos go_to promo


(*
for start i can hold color too, but it shouldn't be needed in the game

because of white moves two up we highlight the col, with black move it resets or changes to blacks two pawn move
and *)
let add_markings (xss : string list list ) : string list list = 
  let rows = ["A";"B";"C";"D";"E";"F";"G";"H"] |> List.map (fun s -> " " ^ s ^ " ")in
  let add_rows = xss  @ [rows]  in
  let add_cols = List.mapi (fun i r -> if i < 8 then (string_of_int (8 - i) ^ " ") :: r else " " :: r) add_rows in
  add_cols

let print_board (b : board) : unit = 
  let modified_board = b |> get_board |> unflatten_board |> list_list_map fig_to_string in
  let with_markings = add_markings modified_board in
  print_list_list Fun.id with_markings

let print_game (g : game) : unit = 
  match g with
  | Playing {board; _} -> print_board board
  | _ -> failwith "game finished"


let highlight_to_string (b,_,_,_ : board) (to_highlight : int list) : string list list = 
  (fun i -> if List.mem i to_highlight then " ● " else List.nth b i |> fig_to_string) 
  |> List.init 64 
  |> unflatten 8
  |> add_markings


let highlight_move_pos (pos : int) (board : board) : unit = 
  let to_highlight = List.map (fun (r,c) -> flatten_pos r c) (possible_moves board pos)  in
  let str_board = highlight_to_string board to_highlight in
  print_list_list Fun.id str_board

let highlight_move (pos : string) (board : board) : unit = 
  let pos = pos_to_int pos in highlight_move_pos pos board

let highlight_in_game (pos : string) (g : game) : unit = 
  match g with
  | Playing {board;_} -> highlight_move pos board
  | _ -> failwith "game ended"

let highlight_all_moves (g : game) : unit = 
  let board = game_to_board g in
  let moves = List.map (fun (r,c) -> flatten_pos r c) (all_moves board) in
  let str_board = highlight_to_string board moves in
  print_list_list Fun.id str_board

let highlight_move_string (pos : string) (g : game)  = 
  let board = game_to_board g in
  let pos = pos_to_int pos in 
  let to_highlight = List.map (fun (r,c) -> flatten_pos r c) (possible_moves board pos)  in
  let str_board = highlight_to_string board to_highlight in
  List.fold_left (fun acc row -> acc ^ List.fold_left (fun acc' x -> acc' ^ x) "" row ^ "\n") "" str_board

let highlight_all_moves_string (g : game) : string = 
  let board = game_to_board g in
  let to_highlight = List.map (fun (r,c) -> flatten_pos r c) (all_moves board)  in
  let str_board = highlight_to_string board to_highlight in
  List.fold_left (fun acc row -> acc ^ List.fold_left (fun acc' x -> acc' ^ x) "" row ^ "\n") "" str_board

let show_board (g : game) = 
  let board = game_to_board g |> get_board |> unflatten_board in
  let str_board = board |> list_list_map fig_to_string  |> add_markings in 
  List.fold_left (fun acc row -> acc ^ List.fold_left (fun acc' x -> acc' ^ x) "" row ^ "\n") "" str_board
(* zamiac wywalac blad powinno nie wywalac, albo powinna byc flaga czy odpalamy z possible moves czy z moves
 *)


(* posprzatać kod jak skończe pisac czesc z szachami *)

(* 
zobrist hashing
transposition tables
*)