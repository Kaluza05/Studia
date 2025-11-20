(** Drzewa binarne. Jeśli wszystkie ścieżki od korzenia do liści mają tę
 * samą długość, to drzewo ma 2ⁿ-1 elementów *)
type 'a tree =
  | Leaf
  | Node of 'a * 'a tree * 'a tree

(** Rzadka reprezentacja liczby: reprezentujemy tylko jedynki i pamiętamy
 * iloma zerami są poprzedzone (patrząc ze strony młodszych cyfr *)
type 'a digits = (int * 'a tree) list

(** Listy skośne *)
type 'a slist =
  (** Liczba zer młodszych od dwójki *)
  { lzeros : int
  (** Dane w dwójce (drzewa mają głębokość lzeros) *)
  ; two    : 'a tree * 'a tree 
  ; rest   : 'a digits
  }

(** Lista pusta *)
let empty =
  { lzeros = 0
  ; two    = (Leaf, Leaf)
  ; rest   = []
  }

(** Zmniejsza odległość do najbliższej jedynki w rzadkiej reprezentacji *)
let decr rest =
  match rest with
  | [] -> []
  | (n, t) :: rest ->
    assert (n > 0);
    (n-1, t) :: rest

(** Zwiększa odległość do najbliższej jedynki *)
let incr rest =
  match rest with
  | [] -> []
  | (n, t) :: rest -> (n+1, t) :: rest

(** Dołączenie elementu z przodu listy *)
let cons x xs =
  let t = Node(x, fst xs.two, snd xs.two) in
  match xs.rest with
  | (0, t') :: rest ->
    { lzeros = xs.lzeros + 1
    ; two    = (t, t')
    ; rest   = rest
    }
  | rest ->
    { lzeros = 0
    ; two    = (Leaf, Leaf)
    ; rest   = (xs.lzeros, t) :: decr rest
    }

(** Widok: None jeśli lista jest pusta, Some(x, xs) dla niepustych list,
 * gdzie x to głowa, a xs to ogon *)
let view xs =
  match xs.two with
  | (Leaf, _) ->
    assert (xs.lzeros = 0);
    begin match xs.rest with
    | []             -> None
    | (_, Leaf) :: _ ->
      (* Przypadek niemożliwy: drzewa w xs.rest zawsze zawierają jakieś dane *)
      assert false
    | (n, Node(x, t1, t2)) :: rest ->
      Some(x,
        { lzeros = n
        ; two    = (t1, t2)
        ; rest   = incr rest
        })
    end
  | (Node(x, t1, t2), t3) ->
    assert (xs.lzeros > 0);
    Some(x, 
      { lzeros = xs.lzeros - 1
      ; two    = (t1, t2)
      ; rest   = (0, t3) :: xs.rest
      })

(** Liczba elementów w drzewie o głębokości rank *)
let tree_size rank =
  (1 lsl rank) - 1

(** n-ty element w drzewie o głębokości rank *)
let rec tree_nth rank t n =
  match t with
  | Leaf -> assert false
  | Node(x, t1, t2) ->
    if n = 0 then x
    else if n - 1 < tree_size (rank - 1) then
      tree_nth (rank - 1) t1 (n - 1)
    else
      tree_nth (rank - 1) t2 (n - 1 - tree_size (rank - 1))

(** n-ty element w ogonie listy, gdzie waga pierwszej cyfry to
  * 2^{rank}-1 *)
let rec rest_nth rank rest n =
  match rest with
  | [] -> raise Not_found
  | (dist, t) :: rest ->
    let rank = rank + dist in
    if n < tree_size rank then
      tree_nth rank t n
    else
      rest_nth (rank + 1) rest (n - tree_size rank)

(** n-ty element na liście skośnej *)
let rec nth xs n =
  let two_size = tree_size xs.lzeros in
  if n < two_size then
    tree_nth xs.lzeros (fst xs.two) n
  else if n < 2 * two_size then
    tree_nth xs.lzeros (snd xs.two) (n - two_size)
  else
    rest_nth (xs.lzeros + 1) xs.rest (n - 2 * two_size)