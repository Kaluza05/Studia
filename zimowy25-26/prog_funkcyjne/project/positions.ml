open Logic


let open_pos1 : board =
  let empty_row = List.init 8 (fun _ -> None) in
  let pawn_row  = List.init 8 (fun _ -> Pawn) in
  let figure_row =
    [Rook;Knight;Bishop;Queen;King;Bishop;Knight;Rook] in
  let add_color c row = List.map (fun x -> Some (c,x)) row in
  let set j v row = List.mapi (fun i x -> if i=j then v else x) row in

  (* rank 8 / 7 — czarne: piony d,e przesunięte *)
  let r8 = add_color Black figure_row in
  let r7 =
    add_color Black pawn_row
    |> set 3 None  (* d7 *)
    |> set 4 None  (* e7 *)
  in

  let r6 = empty_row in

  (* rank 5 — czarne piony na d5,e5 *)
  let r5 =
    empty_row
    |> set 3 (Some (Black, Pawn))
    |> set 4 (Some (Black, Pawn))
  in

  (* rank 4 — białe piony na d4,e4 *)
  let r4 =
    empty_row
    |> set 3 (Some (White, Pawn))
    |> set 4 (Some (White, Pawn))
  in

  let r3 = empty_row in

  (* rank 2 — białe: brak pionów d2,e2 *)
  let r2 =
    add_color White pawn_row
    |> set 3 None
    |> set 4 None
  in

  let r1 = add_color White figure_row in

  (List.flatten [r8;r7;r6;r5;r4;r3;r2;r1], White, (true, true), (true, true), None)

let open_pos2 : board =
  let empty_row = List.init 8 (fun _ -> None) in
  let pawn_row  = List.init 8 (fun _ -> Pawn) in
  let figure_row =
    [Rook;Knight;Bishop;Queen;King;Bishop;Knight;Rook] in
  let add_color c row = List.map (fun x -> Some (c,x)) row in
  let set j v row = List.mapi (fun i x -> if i=j then v else x) row in

  (* rank 8 — czarne: skoczek z g8 → f6, z b8 → c6 *)
  let r8 =
    add_color Black figure_row
    |> set 1 None  (* b8 *)
    |> set 6 None  (* g8 *)
  in

  (* rank 7 — normalna linia pionów poza e7 *)
  let r7 =
    add_color Black pawn_row
    |> set 4 None (* e7 *)
  in

  let r6 =
    empty_row
    |> set 2 (Some (Black, Knight)) (* c6 *)
    |> set 5 (Some (Black, Knight)) (* f6 *)
  in

  (* rank 5 — czarny pion e5 *)
  let r5 =
    empty_row
    |> set 4 (Some (Black, Pawn))
  in

  (* rank 4 — biały pion e4 *)
  let r4 =
    empty_row
    |> set 4 (Some (White, Pawn))
  in

  (* rank 3 — biały skoczek z g1 → f3 *)
  let r3 =
    empty_row
    |> set 5 (Some (White, Knight))
  in

  (* rank 2 — bez piona e2 *)
  let r2 =
    add_color White pawn_row
    |> set 4 None
  in

  (* rank 1 — biały skoczek z b1 → c3 *)
  let r1 =
    add_color White figure_row
    |> set 1 None
  in

  (List.flatten [r8;r7;r6;r5;r4;r3;r2;r1], Black, (true, true), (true, true), None)


let open_pos3 : board =
  let empty_row = List.init 8 (fun _ -> None) in
  let pawn_row  = List.init 8 (fun _ -> Pawn) in
  let figure_row =
    [Rook;Knight;Bishop;Queen;King;Bishop;Knight;Rook] in
  let add_color c row = List.map (fun x -> Some (c,x)) row in
  let set j v row = List.mapi (fun i x -> if i=j then v else x) row in

  let r8 = add_color Black figure_row in

  (* brak pionów d7 i e7 *)
  let r7 =
    add_color Black pawn_row
    |> set 3 None
    |> set 4 None
  in

  (* pion czarny po wymianie z c6 → d5 *)
  let r6 = empty_row in

  let r5 =
    empty_row
    |> set 3 (Some (Black, Pawn))
  in

  (* biały pion z d2→d4 zbijany, pion e2→e4 zostaje na e4 *)
  let r4 =
    empty_row
    |> set 4 (Some (White, Pawn))
  in

  let r3 = empty_row in

  (* brak pionów d2,e2 *)
  let r2 =
    add_color White pawn_row
    |> set 3 None
    |> set 4 None
  in

  let r1 = add_color White figure_row in

  (List.flatten [r8;r7;r6;r5;r4;r3;r2;r1], White, (true, true), (true, true), None)


let open_rooks_pos2 : board =
  let empty_row = List.init 8 (fun _ -> None) in
  let pawn_row  = List.init 8 (fun _ -> Pawn) in
  let figure_row =
    [Rook;Knight;Bishop;Queen;King;Bishop;Knight;Rook] in
  let add_color c row = List.map (fun x -> Some (c,x)) row in
  let set j v row = List.mapi (fun i x -> if i=j then v else x) row in

  (* rank 8 *)
  let r8 = add_color Black figure_row |> set 1 None |> set 2 None in

  (* brak pionów a7 i h7 *)
  let r7 =
    add_color Black pawn_row
    |> set 0 None
    |> set 7 None
  in

  let r6 = empty_row in
  let r5 = empty_row in
  let r4 = empty_row in
  let r3 = empty_row in

  (* brak pionów a2 i h2 *)
  let r2 =
    add_color White pawn_row
    |> set 0 None
    |> set 7 None
  in

  let r1 = add_color White figure_row in

  (List.flatten [r8;r7;r6;r5;r4;r3;r2;r1], White, (true, true), (true, true), None)

let castle_board1 : board =
  let empty_row = List.init 8 (fun _ -> None) in
  let pawn_row  = List.init 8 (fun _ -> Pawn) in
  let figure_row =
    [Rook;Knight;Bishop;Queen;King;Bishop;Knight;Rook] in
  let add_color c row = List.map (fun x -> Some (c,x)) row in
  let set j v row = List.mapi (fun i x -> if i=j then v else x) row in

  (* rank 8 *)
  let r8 = add_color Black figure_row in

  (* brak pionów c7, d7, e7, f7 *)
  let r7 =
    add_color Black pawn_row
    |> set 2 None |> set 3 None |> set 4 None |> set 5 None
  in

  let r6 = empty_row in
  let r5 = List.init 8 (fun i -> if i = 6 then Some (Black,Pawn) else if i = 7 then Some (White, Pawn) else None) in
  let r4 = List.init 8 (fun i -> if i = 3 then Some (White,Queen) else if i = 7 then Some (White, Pawn) else None) in
  let r3 = List.init 8 (fun i -> if i = 5 then Some (Black,Bishop) else None) in

  (* brak pionów c2, d2, e2, f2 *)
  let r2 =
    add_color White pawn_row
    |> set 2 None |> set 3 None |> set 4 None |> set 5 None
  in

  let r1 = add_color White figure_row
    |> set 5 None |> set 6 None in

  (List.flatten [r8;r7;r6;r5;r4;r3;r2;r1], White, (true, true), (true, true), Some(Black,6))


let open_bishops_pos2 : board =
  let empty_row = List.init 8 (fun _ -> None) in
  let pawn_row  = List.init 8 (fun _ -> Pawn) in
  let figure_row =
    [Rook;Knight;Bishop;Queen;King;Bishop;Knight;Rook] in
  let add_color c row = List.map (fun x -> Some (c,x)) row in
  let set j v row = List.mapi (fun i x -> if i=j then v else x) row in

  (* rank 8 *)
  let r8 = add_color Black figure_row in

  (* brak pionów c7, d7, e7, f7 *)
  let r7 =
    add_color Black pawn_row
    |> set 2 None |> set 3 None |> set 4 None |> set 5 None
  in

  let r6 = List.init 8 (fun i -> if i = 0 then Some (Black,Pawn) else None) in
  let r5 = empty_row in
  let r4 = List.init 8 (fun i -> if i = 3 then Some (Black,Knight) else None) in
  let r3 = List.init 8 (fun i -> if i = 4 then Some (White,Knight) else None)  in

  (* brak pionów c2, d2, e2, f2 *)
  let r2 =
    add_color White pawn_row
    |> set 2 None |> set 3 None |> set 4 None |> set 5 None
  in

  let r1 = add_color White figure_row in

  (List.flatten [r8;r7;r6;r5;r4;r3;r2;r1], White, (true, true), (true, true), None)
  

let curr_game = game |> move "E2" "E4"  |> move "E7" "E6" |> move "E4" "E5" |> move "D7" "D5" |> move "E5" "D6"