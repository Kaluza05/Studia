type t =
  | UInt   of int
  | UConst of string
  | UApp   of t * t

type token =
  | BrOpn
  | BrCls
  | Num of int
  | Id  of string

let is_digit c = c >= '0' && c <= '9'
let is_var c =
  (c >= 'a' && c <= 'z') ||
  (c >= 'A' && c <= 'Z') ||
  is_digit c ||
  c = '_' || c = '*' || c = '+' || c = '/' || c = '-'
let is_space c = c = ' ' || c = '\n' || c = '\t' || c = '\r'

let value_of_digit c =
  Char.code c - Char.code '0'

let rec tokenize_num n str pos =
  if pos = String.length str || not (is_digit str.[pos]) then (pos, n)
  else
    let n = n * 10 + value_of_digit str.[pos] in
    tokenize_num n str (pos+1)

let rec tokenize_id buf str pos =
  if pos = String.length str || not (is_var str.[pos]) then pos
  else begin
    Buffer.add_char buf str.[pos];
    tokenize_id buf str (pos+1)
  end

let rec tokenize_loop tok str pos =
  if pos = String.length str then List.rev tok
  else
    let c = str.[pos] in
    let pos = pos + 1 in
    match c with
    | c when is_space c -> tokenize_loop tok str pos
    | '(' -> tokenize_loop (BrOpn :: tok) str pos
    | ')' -> tokenize_loop (BrCls :: tok) str pos
    | n when is_digit n ->
      let (pos, n) = tokenize_num (value_of_digit n) str pos in
      tokenize_loop (Num n :: tok) str pos
    | c when is_var c ->
      let buf = Buffer.create 32 in
      Buffer.add_char buf c;
      let pos = tokenize_id buf str pos in
      tokenize_loop (Id (Buffer.contents buf) :: tok) str pos
    | _ -> failwith "Parse error"

let tokenize str =
  tokenize_loop [] str 0

let rec parse_expr tok =
  let (hd, tok) = parse_atom tok in
  parse_app hd tok

and parse_app hd tok =
  match try_parse_atom tok with
  | None -> (hd, tok)
  | Some(at, tok) -> parse_app (UApp(hd, at)) tok

and try_parse_atom tok =
  match tok with
  | [] | BrCls :: _ -> None
  | Num n :: tok -> Some(UInt n, tok)
  | Id  x :: tok -> Some(UConst x, tok)
  | BrOpn :: tok ->
    let (e, tok) = parse_expr tok in
    begin match tok with
    | BrCls :: tok -> Some (e, tok)
    | _ -> failwith "Parse error"
    end

and parse_atom tok =
  match try_parse_atom tok with
  | Some r -> r
  | None -> failwith "Parse error"

let parse str =
  match parse_expr (tokenize str) with
  | (e, []) -> e
  | _ -> failwith "Parse error"
