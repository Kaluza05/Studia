(*kolejnosc jest troche zla doklejania*)

type (_,_) format = 
  | Lit : string -> ('a, 'a) format
  | Int : (int -> 'a, 'a) format
  | Str : (string -> 'a, 'a) format
  | Cat : (('a,'b) format) * ('b,'c) format -> ('a,'c) format

let (^^) x y = Cat(x,y)

let rec ksprintf : type a b. (a, b) format -> (string -> b) -> a = 
  fun fmt cont -> 
  match fmt with
  | Lit(s)       -> cont s
  | Int          -> fun i -> cont (string_of_int i)
  | Str          -> fun s -> cont s
  | Cat(dr1,dr2) -> ksprintf dr1 (fun s1 -> ksprintf dr2 (fun s2 -> cont (s1 ^ s2)))

let sprintf (fmt : ('a,string) format) : 'a = ksprintf fmt (fun s -> s)

let kprintf : type a b. (a, b) format -> b -> a = 
  fun fmt ret -> 
  let rec kprintf : type a b. (a, b) format -> (unit -> b) -> a = 
    fun fmt cont -> 
    match fmt with
    | Lit(s) -> 
        print_string s;
        cont ()
    | Int    -> 
      fun i  -> 
        print_string (string_of_int i);
        cont ()
    | Str    ->
      fun s  ->
        print_string s;
        cont ()
    | Cat(dr1,dr2) -> kprintf dr1 (fun () -> kprintf dr2 cont)

  in kprintf fmt (fun () -> ret)

(* trzeba odroczyć wywolanie kprintf dr2 ret dopoki nie przekaze się wszystkich arguemntow do kprintf dr1 r*)

(*printowane na odwrot bo najpierw wykonuje kprintf dr2 ret pewnie, dla Lit wykonuje się od razu*)
let printf (fmt : ('a, unit) format) : 'a = kprintf fmt ()

let a = printf (Int ^^ Str ^^ Lit "dce ") 2 " av"