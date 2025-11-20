type sock

val output_string : sock -> string -> unit

val input_line : sock -> string option

val close : sock -> unit

val establish_server : port:int -> (sock -> unit) -> unit
