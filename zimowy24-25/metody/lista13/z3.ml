let rec insert xs x =
  match xs with
  | [] -> [x]
  | y :: xs' -> if x >= y then y :: insert xs' x else x :: xs

let rec insert_sort xs =
  match xs with
  | [] -> []
  | x::xs -> insert (insert_sort xs) x
