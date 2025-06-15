%{
open Ast
%}

%token <string> IDENT
%token TOP
%token BOT
%token AND
%token OR
%token NOT
%token FORALL
%token EXISTS
%token LPAREN
%token RPAREN
%token EOF

%start <Ast.qbf> main

%left AND OR
%right FORALL EXISTS
%right NOT

%%

main:
    | e = expr; EOF { e }
    ;


expr:
    | x = IDENT                 { Var x         }
    | TOP                       { Top           }
    | BOT                       { Bot           }
    | e1 = expr; AND; e2 = expr { Conj (e1, e2) }
    | e1 = expr; OR; e2 = expr  { Disj (e1, e2) }
    | NOT; e=expr               { Not e         }
    | FORALL; i = IDENT; e=expr { Forall (i, e) }
    | EXISTS; i = IDENT; e=expr { Exists (i, e) }
    | LPAREN; e = expr; RPAREN  { e             }
    ;


