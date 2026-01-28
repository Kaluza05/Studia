open Gametype

let nth_empty i xs empty_value = 
  let rec it i xs acc = 
  match xs with
  | [] -> failwith "shouldnt be empty"
  | x :: xs when snd x = empty_value -> if i = 0 then acc else it (i-1) xs (acc + 1)
  | _ :: xs -> it i xs (acc + 1)
  
  in it i xs 0


(**returns random integer from \[a,b]*)
let random_from_range a b = a + Random.int (b-a + 1)
let random_from_list xs = let n = Random.int (List.length xs) in List.nth xs n 




module Chess960 : Game = struct
  open Logic
  (* last three ints represent starting position of king,rook queenside, rook kingside *)
  type position = Logic.position
  type board = position * Logic.color * Logic.castle_rights * ((Logic.color * int) option) * (int * int * int)

  type move = string * string
  type repetition_tracker = (int,int) Hashtbl.t
  type no_takes_tracker = int
  type coding = string list
  type color = Logic.color

  type game_representation = 
    {
    board : board;
    reperirtion_tracker : repetition_tracker;
    no_takes_tracker : no_takes_tracker;
    coding : coding
    }
  
  type game = 
  | Playing of game_representation
  | Win of color
  | Draw


  let get_board ((b,turn,cast,en,_) : board) : Logic.board = (b,turn,cast,en)

  let init_game () =
    let shuffle_figures () = 
    (* zapisujemy ozycje w fen 
    losujemy na pozycji od 1 do 6 (nie moze stac na 0 i 7)
    losujemy pozycje wieży od 0,poz_krola - 1, poz_krola+1, 7
    losujemy pozycje dla gońca, pozniej dla gońca na przeciwynm kolorze,
    losujemy dla damy
    losujemy dla skoczkow*)
    let placed = ref (List.init 8 (fun i -> (i,""))) in 
    let put_at_n n x = placed := List.mapi (fun i x' -> if i = n then (i,x) else x') !placed in

    let king_pos    = random_from_range 1 6 in              (*from 1 to 6*)
    put_at_n king_pos "k";
    
    let rook_pos1   = random_from_range 0 (king_pos - 1)   in (*from 0 to king_pos - 1*)
    put_at_n rook_pos1 "r";
    
    let rook_pos2   = random_from_range (king_pos + 1) 7 in (*from king_pos + 1 to 7*)
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
            reperirtion_tracker = Hashtbl.create 128; no_takes_tracker = 0; coding = []}

  let visible (col : color) (g : game) : position = 
  match g with
  | Playing {board;_} -> 
    let b,c,cast,en,_ = board in 
    let pieces = Logic.all_pieces (b,c,cast,en) col in (*pozycje wszystkich naszych figur, dla kazdej znalezc co atakuje*)
    let attacked = List.map (fun (r,c) -> Logic.flatten_pos r c) (List.concat_map (Logic.attacked_positions (b,c,cast,en)) pieces) in
    let visible = pieces @ attacked in 
    List.mapi (fun i x -> if List.mem i visible then x else None) b
    

  | _ -> failwith "nothing visible game ended"

(* mozna roszade jak pola po ktorych krol przejdzie nie sa atakowane oraz jak nic tam nie stoi oprocz naszej wiezy *)
let can_castle_kingside (brd : board) (col : color) : bool =
  let b,t,castle_rights,_,(k,r1,r2) = brd in 
  let brd = get_board brd in
  let king_pos = locate_figure brd (col,King) in 
  (* calculate neccessary positions*)
  match col with
  | White -> 
    let rook_pos = flatten_pos 7 r2 in 
  let king_traverse = make_range king_pos 62 in (*list of squares on which the king will step from king pos to g1 to 62*)
  List.for_all (fun i -> not (is_attacked brd i !!t)) king_traverse 
    && castle_rights.white_kingside && List.nth b rook_pos = Some(White,Rook) &&
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
  match List.nth b curr_pos with
  | Some(_,King) -> 
    let _,_   = unflatten_pos curr_pos
    and _',col' = unflatten_pos go_to
    in if col' = 6 || col' = 2 then   
      let rook_go = if col' = 6 then curr_pos  + 1 else curr_pos - 1 in      (*position for rook to go on*)
      let rook_pos = if col' = 6 then r2 else r1 in (*ruccent rook pos*)
      let b = insert_at_n b None rook_pos in
      let b = insert_at_n b None curr_pos in
      let b = insert_at_n b (Some(t,King)) go_to in 
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
  (curr_pos : int) (go_to : int)  =
  let board960 = (b,t,castle,e,st) in
  let brd = get_board board960 in
  (if is_valid board960 curr_pos go_to |> not then 
  if not from_possible_moves then raise (Logic.InvalidMove(Logic.int_to_pos go_to)) else raise (Logic.CheckAfterMove("s"))
  else
  let* () =  try_double_pawn_move brd curr_pos go_to in
  let* () =  try_enpassant brd curr_pos go_to in
  let* () =  try_castle board960 curr_pos go_to    in
  let* () =  try_promotion brd curr_pos go_to promo in
  let* () =  update_regular_move brd curr_pos go_to in
  ExceptionMonad.fail)
  |> Option.get

  let move = failwith "c"


end


(* trzeba wybrac co ma byc klikniete do roszady:
pozycja na ktora ma ich krol?
kliknac na wieze?

zrobie chyba zeby kliknac na wieze
 *)


(* wieza juz na f1 -> f1 *)