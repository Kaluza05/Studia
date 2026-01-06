open Logic

type bot_game

val bot_color : bot_game -> color

val move_player : string -> string -> bot_game -> bot_game

val game_with_bot : color -> int -> bot_game

val bot_game_to_game : bot_game -> game