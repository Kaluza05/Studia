type 'a symbol =
| T of string (* symbol terminalny *)
| N of 'a (* symbol nieterminalny *)

type 'a grammar = ('a * ('a symbol list ) list ) list

let pol : string grammar =
    [ " zdanie ", [[ N "grupa - podmiotu "; N "grupa - orzeczenia "]]
    ; "grupa - podmiotu ", [[ N " przydawka "; N " podmiot "]]
    ; "grupa - orzeczenia ", [[ N " orzeczenie "; N " dopelnienie "]];
     " przydawka ",     [[ T " Piekny "];
                        [ T " Bogaty "];
                        [ T " Wesoly "]]
    ; " podmiot ",      [[ T " policjant "];
                        [ T " student "];
                        [ T " piekarz "]]
    ; " orzeczenie ",   [[ T " zjadl "];
                        [ T " pokochal "];
                        [ T " zobaczyl "]]
    ; " dopelnienie ",  [[ T " zupe ."];
                        [ T " sam siebie ."];
                        [ T " instytut informatyki ."]]
    ]
    
let n_th_elem (l: 'a list) (n:int) : 'a =
    let rec it xs k =
        match  xs  with
        | [] -> failwith "lista pusta"
        | x::xs' -> if k = n then x else it xs' (k+1)
    in it l 0;;

let rec get_prod (grammar : 'a grammar) (non_terminal : 'a) : ('a symbol list) =
    match grammar with
    | [] -> []
    | (n,prods) :: grammar' -> if n = non_terminal then n_th_elem prods (prods |> List.length |>Random.int) 
        else get_prod grammar' non_terminal

let  generate (grammar : 'a grammar) (start : 'a) : string =
    let rec go_through_prod (production : 'a symbol list) : string=
        match production with
        | [] -> ""
        | N non_terminal :: rest -> (go_through_prod (get_prod grammar non_terminal)) ^ (go_through_prod rest )
        | T terminal :: rest -> terminal ^ go_through_prod rest 
    in go_through_prod (get_prod grammar start)


let expr : unit grammar =
    [() , [[ N () ; T "+"; N () ];
    [ N () ; T "*"; N () ];
    [ T "("; N () ; T ")"];
    [ T "n" ]]]
        
let example_grammar : string grammar = [
    ("A", [[N "A"; T "b"];[T "c"]])
]

let ex_gram = ["S",[[T "(" ; N "T"; T ")"; N "S"];[T "(" ;N "S"];[T ""]];"T", [[T "(" ; N "T"; T ")"; N "T"];[T ""]]]