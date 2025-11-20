type t =
  | UInt   of int
  | UConst of string
  | UApp   of t * t

val parse : string -> t
