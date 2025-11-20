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

let rec kprintf : type a b. (a, b) format -> b -> a = 
  fun fmt ret -> 
  match fmt with
  | Lit(s) -> 
      print_string s;
      ret
  | Int    -> 
    fun i  -> 
      print_string (string_of_int i);
      ret
  | Str    ->
    fun s  ->
      print_string s;
      ret
  | Cat(dr1,dr2) -> kprintf dr1 (kprintf dr2 ret)

(*printowane na odwrot bo najpierw wykonuje kprintf dr2 ret pewnie, dla Lit wykonuje się od razu*)
let printf (fmt : ('a, unit) format) : 'a = kprintf fmt ()

let a = printf (Int ^^ Str ^^ Lit "wallam") 2 " av"