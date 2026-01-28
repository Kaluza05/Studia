module type Game = sig
  type board
  type repetition_tracker
  type no_takes_tracker
  type coding
  type move

  type game_representation = 
    {
    board : board;
    reperirtion_tracker : repetition_tracker;
    no_takes_tracker : no_takes_tracker;
    coding : coding
    }

  type game

  val init_game : unit -> game
  val move : move -> game -> game
end

(* do fog of war dochodzi tylko, że pokazujemy tylko pola na ktoych stoimi i ktore atakujemy
en passant nie moze bić jeśli nie widać pionka 
wygrywa sie bijąc króla*)


(* do chess960 trzeba zmienic tylko inicjalizację pozycji i zmiana w roszadzie *)

(* 
let test n = 
  for i = 0 to n do
    if List.length (init_game ()) <> 8 then failwith "zla dlugosc"
    else ()
  done *)

open Logic

(* 
pozycje od króla do wieży, wziac pozycje wież
zapisz pozycje gdzie startuje król i obie wieże

zeby moc zrobić roszadę kingside zobacz pozycję krol, wieza i jesli nic oprocz tej wiezy nie ma po drodze
od king_pos -> g1 to można
i pozycja na ktora wieza idzie ma byc pusta



*)

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



(* 
do fog of war zmienić ze nie trzeba wykrywac szacha dalej mimo szacha można się poruszać innymi figurami
zmienić co jest wysyłane do serwera

*)

