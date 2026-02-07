open Logic
(* last three ints represent starting position of king,rook queenside, rook kingside *)
type position = Logic.position
type color = Logic.color
type board = position * color * Logic.castle_rights * ((Logic.color * int) option) * (int * int * int)
type move = string * string
type repetition_tracker = (int,int) Hashtbl.t
type no_takes_tracker = int
type coding = string list

type game_representation = 
  {
  board : board;
  repetition_tracker : repetition_tracker;
  no_takes_tracker : no_takes_tracker;
  coding : coding
  }

type game = 
| Playing of game_representation
| Win of color
| Draw

let nth_empty i xs empty_value = 
  let rec it i xs acc = 
  match xs with
  | [] -> failwith "shouldnt be empty"
  | x :: xs when snd x = empty_value -> if i = 0 then acc else it (i-1) xs (acc + 1)
  | _ :: xs -> it i xs (acc + 1)
  in it i xs 0


(**returns random integer from \[a,b]*)
let random_from_range (a : int) (b : int) : int = a + Random.int (b-a + 1)
let random_from_list xs = let n = Random.int (List.length xs) in List.nth xs n 

let get_board ((b,turn,cast,en,_) : board) : Logic.board = (b,turn,cast,en)




let get_turn (g : game) = 
  match g with
  | Playing {board;_}-> let _,t,_,_,_ = board in t
    | _ -> failwith "game ender when converting to turn 960"
let game_to_position (g : game) = 
  match g with
  | Playing {board;_}-> let p,_,_,_,_ = board in p
  | _ -> failwith "game ender when converting to position 960"


let init_game () =
  let shuffle_figures () = 
  (* zapisujemy pozycje w fen 
  losujemy na pozycji od 1 do 6 (nie moze stac na 0 i 7)
  losujemy pozycje wieży od 0,poz_krola - 1, poz_krola+1, 7
  losujemy pozycje dla gońca, pozniej dla gońca na przeciwynm kolorze,
  losujemy dla damy
  losujemy dla skoczkow
  *)
  let placed = ref (List.init 8 (fun i -> (i,""))) in 
  let put_at_n n x = placed := List.mapi (fun i x' -> if i = n then (i,x) else x') !placed in
  let king_pos    = random_from_range 1 6 in
  put_at_n king_pos "k";
  
  let rook_pos1   = random_from_range 0 (king_pos - 1)   in
  put_at_n rook_pos1 "r";
  
  let rook_pos2   = random_from_range (king_pos + 1) 7 in
  put_at_n rook_pos2 "r";
  let bishop_pos1 = nth_empty (random_from_range 0 4) !placed "" in (*bishop at some empty square*)
  put_at_n bishop_pos1 "b";
  let bishop_pos2 =   (* bishop at some empty square of different color *)
    let free_for_bishop2 = List.filter (fun i -> 
      i mod 2 <> bishop_pos1 mod 2 && i <> king_pos 
      && i <> rook_pos1 && i <> rook_pos2
      ) 
    (List.init 8 (fun i -> i)) in
    random_from_list free_for_bishop2 in 
  put_at_n bishop_pos2 "b";
  let queen_pos   = nth_empty (random_from_range 0 2) !placed "" in (*queen at some empty square*)
  put_at_n queen_pos "q";
  let knight_pos1 = nth_empty (random_from_range 0 1) !placed "" in (*knight at some empty square*)
  put_at_n knight_pos1 "n";
  let knight_pos2 = nth_empty 0 !placed "" in                        (*knight at last empty square*)
  put_at_n knight_pos2 "n";
  !placed |> List.map snd |> List.fold_left (fun x acc -> x ^ acc) "", king_pos, rook_pos1, rook_pos2
  in
  let last_row,king_pos, rook_pos1, rook_pos2 = shuffle_figures () in
  let pawn_row = String.init 8 (Fun.const 'p') in
  let board = String.concat "/" (
    [last_row;
    pawn_row;
    "8";
    "8";
    "8";
    "8";
    String.map Char.uppercase_ascii pawn_row;
    String.map Char.uppercase_ascii last_row])
    in
    Playing {board = Logic.fen_to_board board, Logic.White,Logic.{white_kingside = true;white_queenside = true;
          black_kingside = true;black_queenside = true} , None, (king_pos,rook_pos1,rook_pos2);
          repetition_tracker = Hashtbl.create 128; no_takes_tracker = 0; coding = []}

let visible (_ : color) (g : game) = g |> game_to_position

(* mozna roszade jak pola po ktorych krol przejdzie nie sa atakowane oraz jak nic tam nie stoi oprocz naszej wiezy *)
let can_castle_kingside (brd : board) (col : color) : bool =
  let b,t,castle_rights,_,(_,_,r2) = brd in 
  let brd = get_board brd in
  let king_pos = locate_figure brd (col,King) in 
  (* calculate neccessary positions*)
  match col with
  | White -> 
  let king_traverse = make_range king_pos 62 in (*list of squares on which the king will step from king pos to g1 to 62*)
  let rook_traverse = make_range (r2+56)  61 in
  List.for_all (fun i -> not (is_attacked brd i !!t)) king_traverse 
    && castle_rights.white_kingside 
    && List.for_all (fun i -> List.nth b i |> Option.is_none) (List.filter (fun i -> i <> 56 + r2 && i <> king_pos) king_traverse)
    && List.for_all (fun i -> List.nth b i |> Option.is_none) (List.filter (fun i -> i <> 56 + r2 && i <> king_pos) rook_traverse) 
    && (if king_pos = 62 then if r2 = 5 then true else List.nth b 61 |> Option.is_none else true)
  | Black -> 
    let king_traverse = make_range king_pos 6 in
    let rook_traverse = make_range r2  5 in
    List.for_all (fun i -> not (is_attacked brd i !!t)) king_traverse
    && castle_rights.black_kingside 
    && List.for_all (fun i -> List.nth b i |> Option.is_none) (List.filter (fun i -> i <> r2 && i <> king_pos) king_traverse)
    && List.for_all (fun i -> List.nth b i |> Option.is_none) (List.filter (fun i -> i <> r2 && i <> king_pos) rook_traverse)
    && (if king_pos = 6 then if r2 = 5 then true else List.nth b 5 |> Option.is_none else true)

let can_castle_queenside (brd : board) (col : color) : bool =
  let b,t,castle_rights,_,(_,r1,_) = brd in 
  let brd = get_board brd in
  let king_pos = locate_figure brd (col,King) in 
  (* calculate neccessary positions*)
  match col with
  | White -> 
  let king_traverse = make_range king_pos 58 in (*list of squares on which the king will step from king pos to g1 to 62*)
  let rook_traverse = make_range (56+r1) 59 in
  List.for_all (fun i -> not (is_attacked brd i !!t)) king_traverse 
    && castle_rights.white_kingside 
    && List.for_all (fun i -> List.nth b i |> Option.is_none) (List.filter (fun i -> i <> 56 + r1 && i <> king_pos) king_traverse)
    && List.for_all (fun i -> List.nth b i |> Option.is_none) (List.filter (fun i -> i <> 56 + r1 && i <> king_pos) rook_traverse) 
    && (if king_pos = 58 then if r1 = 3 then true else List.nth b 61 |> Option.is_none else true)
  | Black -> 
    let king_traverse = make_range king_pos 2 in
    let rook_traverse = make_range r1 3 in
    List.for_all (fun i -> not (is_attacked brd i !!t)) king_traverse
    && castle_rights.black_kingside 
    && List.for_all (fun i -> List.nth b i |> Option.is_none) (List.filter (fun i -> i <> r1 && i <> king_pos) king_traverse)
    && List.for_all (fun i -> List.nth b i |> Option.is_none) (List.filter (fun i -> i <> r1 && i <> king_pos) rook_traverse)
    && (if king_pos = 2 then if r1 = 3 then true else List.nth b 5 |> Option.is_none else true)


let add_castle_kingside (b : board) (col : color) = 
  let _,_,_,_,(_,_,r2) = b in
  if can_castle_kingside b col
    then match col with
    | White -> [(7,r2)]
    | Black -> [(0,r2)]
  else []
let add_castle_queenside (b : board) (col : color) =
  let _,_,_,_,(_,r1,_) = b in
  if can_castle_queenside b col
    then match col with
    | White -> [(7,r1)]
    | Black -> [(0,r1)]
  else []

let add_castle (b : board) (col : color) : (int * int) list = add_castle_kingside b col @ add_castle_queenside b col

let add_king ((b,turn,cast,en,st) : board) (pos : int) = 
  let board960 = (b,turn,cast,en,st) in 
  let board = get_board board960 in
  let t = (List.nth b pos) |> Option.get |> fst in
  let moves = Logic.king_attacks board pos in
  let moves_castle = add_castle board960 t @ moves in
  moves_castle


  let possible_for_king   (b : board)  (pos : int) = add_king b pos


  let  possible_moves_helper ((b,turn,cast,en,st) : board) (pos : int) : (int * int) list = 
  let board960 = (b,turn,cast,en,st) in 
  let board = get_board board960 in
  let moves = match List.nth b pos with 
  | None -> [] (*failwith "can't chose empty square poss_moves"*)
  | Some (c,f) when c = turn ->
    begin match f with
    | Pawn -> 
      begin match c with
      | White  -> Logic.possible_for_wpawn  board pos
      | Black  -> Logic.possible_for_bpawn  board pos
      end
    | Rook     -> Logic.possible_for_rook   board pos
    | Knight   -> Logic.possible_for_knight board pos
    | Bishop   -> Logic.possible_for_bishop board pos
    | King     -> possible_for_king   board960 pos
    | Queen    -> Logic.possible_for_queen  board pos
    (*filtrowac jesli ruch mialby spowodowac szach na naszym królu*)
    end
  | Some _ -> []
  in moves

  

  let try_castle (b,t,castle_rights,_,(_,r1,r2) : board) (curr_pos : int) (go_to : int) = 
  let some_num = match t with White -> 56 | Black -> 0
  in
  match List.nth b curr_pos with
  | Some(_,King) -> 
    let _,_   = unflatten_pos curr_pos
    and _',col' = unflatten_pos go_to
    in if col' = r1 || col' = r2 then   
      let king_go = if col' = r1 then some_num + 2 else some_num + 6 in      (*position for rook to go on*)
      let rook_go = if col' = r1 then king_go + 1 else king_go - 1 in
      let rook_pos = if col' = r1 then some_num + r1 else some_num + r2 in (*ruccent rook pos*)
      let b = insert_at_n b None rook_pos in
      let b = insert_at_n b None curr_pos in
      let b = insert_at_n b (Some(t,King)) king_go in 
      let b = insert_at_n b (Some(t,Rook)) rook_go in
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


  let is_valid (b,t,castle,e,st : board) (curr_pos : int) (go_to : int) = 
  (match List.nth b curr_pos with
  | Some(c,_) when c = t -> true
  | _ -> false)
  && List.mem (Logic.unflatten_pos go_to) (possible_moves_helper (b,t,castle,e,st) curr_pos) 


  let move_helper1 ?(from_possible_moves = false) ?(promo = Logic.Queen) (b,t,castle,e,st : board) 
  (curr_pos : int) (go_to : int) : board =
  let board960 = (b,t,castle,e,st) in
  let brd = get_board board960 in
  let p,t,cst,en = (if is_valid board960 curr_pos go_to |> not then 
  if not from_possible_moves then raise (Logic.InvalidMove(Logic.int_to_pos go_to)) else raise (Logic.CheckAfterMove("s"))
  else
  let* () =  try_double_pawn_move brd curr_pos go_to in
  let* () =  try_enpassant brd curr_pos go_to in
  let* () =  try_castle board960 curr_pos go_to    in
  let* () =  try_promotion brd curr_pos go_to promo in
  let* () =  update_regular_move brd curr_pos go_to in
  OptionMonad.fail)
  |> Option.get
  in p,t,cst,en,st

let possible_moves (board : board) (pos : int) : (int * int) list = 
  let _,t,_,_,_ = board in 
  let moves = possible_moves_helper board pos 
    in
    List.filter (fun (r,c) -> 
      try 
      let after_move = move_helper1 ~from_possible_moves:true board pos (flatten_pos r c) in 
      is_check (get_board after_move) t |> not
      with
      | CheckAfterMove(_) -> false
      | e -> raise e 
      ) 
      moves

  let game_repr_to_state (g : game_representation) = 
    {board = get_board g.board;repetitions = g.repetition_tracker; no_takes_counter = g.no_takes_tracker; coding = g.coding}

  let move_helper2 ({board;repetition_tracker; no_takes_tracker;coding} : game_representation) 
  (curr_pos : int) (go_to : int) (promo : piece) : game = 
  let board960 = board in
  let board = get_board board960 in
  let p,_,_,_ = board in
  let no_takes_tracker = if is_pawn_move p curr_pos go_to || is_capture p curr_pos go_to then 0 else no_takes_tracker + 1 in
  let coding = coding in
  let board = move_helper1 board960 curr_pos go_to ~promo in
  (* add_board repetitions board; *)
  let game_state = {board;repetition_tracker;no_takes_tracker;coding} in
  let _,t,_,_,_ = board in 
  if is_check (get_board board) (!!t) then raise (CheckAfterMove "check after move") else
  if is_checkmate (get_board board) then Win !!t else
  if is_draw (game_repr_to_state game_state) then Draw else
  (* update counters *)
  (* juz nie sprawdzamy szachów i matów 
  ,sprawdzamy jednak pojebalo mi sie z fog of war*)
  Playing game_state

let move_int ?(promo = Queen) (curr_pos : int) (go_to : int) (g : game) : game =
  match g with
  | Playing game_state -> 
    move_helper2 game_state curr_pos go_to promo
  | Win c -> failwith ("game finished with player" ^ string_of_color c ^ "winning")
  | Draw -> failwith "game ended with a draw, can't move int"


  let move ?(promo = Queen) (curr_pos,go_to : move) (g : game) : game =
  let curr_pos, go_to = pos_to_int curr_pos, pos_to_int go_to in
  move_int curr_pos go_to g ~promo:promo

  let move_opt ?(promo : piece = Queen) (curr_pos, go_to : move) (g : game) : game option =
  try
  let curr_pos, go_to = pos_to_int curr_pos, pos_to_int go_to in
  Some (move_int curr_pos go_to g ~promo:promo)
  with
  | WrongPos _ -> None
  | InvalidMove _ -> None
  | CheckAfterMove _ -> None

let highlight_squares (pos : string) (g : game) = 
  let p = pos_to_int pos in
  match g with
  | Win _ | Draw -> failwith "game ended can highlight"
  | Playing {board; _} -> 
    possible_moves board p
    |> List.map (fun (r,c) -> flatten_pos r c)

