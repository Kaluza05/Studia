%{
open Ast
%}

%token <bool> BOOL
%token <int> INT
%token <string> IDENT
%token <unit> UNIT
%token IF
%token THEN
%token ELSE
%token LET
%token IN
%token PLUS
%token MINUS
%token TIMES
%token DIV
%token AND
%token OR
%token EQ
%token LEQ
%token LPAREN
%token RPAREN
%token COMMA
%token FST
%token SND
%token MATCH
%token WITH
%token ARROW
%token SUM
%token TO
%token EOF

%start <Ast.expr> main

%left AND OR
%nonassoc EQ LEQ
%left PLUS MINUS
%left TIMES DIV
%right FST SND

%%

main:
    | e = mexpr; EOF { e }
    ;

mexpr:
    | IF;    e1 = mexpr ; THEN; e2 = mexpr ; ELSE;  e3 = mexpr                     { If (e1,e2,e3) }
    | LET;   x = IDENT  ; EQ;   e1 = mexpr ; IN;    e2 = mexpr                     { Let (x,e1,e2) }
    | SUM;   x = IDENT  ; EQ;   e1 = mexpr ; TO;    e2 = mexpr; IN    ; e3 = mexpr { Sum (x,e1,e2,e3) }
    | MATCH; e1 = mexpr ; WITH; i1 = IDENT ; COMMA; i2 = IDENT; ARROW ; e2 = mexpr { Match (e1,i1,i2,e2) }
    | e = expr { e }
    ;

expr:
    | i = INT   { Int i  }
    | b = BOOL  { Bool b }
    | x = IDENT { Var x  }
    | u = UNIT  { Unit u }
    | e1 = expr; PLUS  ; e2 = expr { Binop (Add, e1, e2 )  }
    | e1 = expr; MINUS ; e2 = expr { Binop (Sub, e1, e2 )  }
    | e1 = expr; DIV   ; e2 = expr { Binop (Div, e1, e2 )  }
    | e1 = expr; TIMES ; e2 = expr { Binop (Mult, e1, e2)  }
    | e1 = expr; AND   ; e2 = expr { Binop (And, e1, e2 )  }
    | e1 = expr; OR    ; e2 = expr { Binop (Or, e1, e2  )  }
    | e1 = expr; EQ    ; e2 = expr { Binop (Eq, e1, e2  )  }
    | e1 = expr; LEQ   ; e2 = expr { Binop (Leq, e1, e2 )  }
    | FST; e = expr                {Pairop (Fst, e )       }
    | SND; e = expr                {Pairop (Snd, e )       }
    | LPAREN; e = mexpr; RPAREN { e }
    | LPAREN; e1 = mexpr; COMMA; e2 = mexpr; RPAREN {Pair (e1, e2)}
    ;


