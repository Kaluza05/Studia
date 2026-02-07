type game_type

val game_type_by_game : game_type -> string

val starting_board    : string -> game_type

val make_move         : game_type -> string -> string -> game_type option

val is_draw           : game_type -> bool

val is_win            : game_type -> bool

val get_winner        : game_type -> Logic.color

val get_turn          : game_type -> Logic.color

val game_to_position  : game_type -> Logic.position

val game_to_visible   : Logic.color -> game_type -> string

val highlight_squares : string -> game_type -> int list