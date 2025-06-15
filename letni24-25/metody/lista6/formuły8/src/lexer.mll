{
open Parser
}

let white = [' ' '\t']+
let digit = ['0'-'9']
let char = ['a'-'z' 'A'-'Z' '_']
let ident = char(char|digit)*

rule read =
    parse
    | white { read lexbuf }
    | "1" { TOP    }
    | "0" { BOT    }
    | "^" { AND    }
    | "v" { OR     }
    | "!" { NOT    }
    | "A" { FORALL }
    | "E" { EXISTS }
    | "(" { LPAREN }
    | ")" { RPAREN }
    | ident { IDENT (Lexing.lexeme lexbuf) }
    | eof { EOF }
