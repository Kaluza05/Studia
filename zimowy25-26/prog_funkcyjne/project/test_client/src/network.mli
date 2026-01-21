type ans
type sock

val output_string : sock -> string -> (unit -> ans) -> ans

val input_line : sock -> (string option -> ans) -> ans

val close : sock -> unit

val exit : unit -> ans

val spawn : (unit -> ans) -> unit

val establish_server : port:int -> (sock -> ans) -> unit

val connect : string -> int -> sock