{
open Parser
}

let white = [' ' '\t']+
let digit = ['0'-'9']
let number = '-'? digit+
let char = ['a'-'z' 'A'-'Z' '_']
let ident = char(char|digit)*

rule read =
    parse
    | white     { read lexbuf }
    | "*"       { TIMES }
    | "+"       { PLUS }
    | "-"       { MINUS }
    | "/"       { DIV }
    | "&&"      { AND }
    | "||"      { OR }
    | "="       { EQ }
    | "<="      { LEQ }
    | "("       { LPAREN }
    | ")"       { RPAREN }
    | "if"      { IF }
    | "then"    { THEN }
    | "let"     { LET }
    | "in"      { IN }
    | "else"    { ELSE }
    | "()"      { UNIT () }
    | "true"    { BOOL true }
    | "false"   { BOOL false }
    | ","       { COMMA }
    | "fst"     {FST}
    | "snd"     {SND}
    | "sum"     { SUM }
    | "to"      { TO }
    | "match"   { MATCH}
    | "with"    { WITH}
    | "->"      { ARROW}
    | number    { INT ( int_of_string (Lexing.lexeme lexbuf)) }
    | ident     { IDENT (Lexing.lexeme lexbuf) }
    | eof       { EOF }
