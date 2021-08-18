(** Parsing of player commands*)

type location_phrase = string array
(** [location_phrase] is a two element string array that represents a move from
the location specefied by the first element to the location specefied by the 
second element*)

(** [game_type_command] are the types of chess games that can be 
selected by the player(s) (One, Two, Sim ) and an option to quit a game (Q)*)
type game_type_command = One | Two | Q | Sim | Sim2 of int

type depth = int
(** [depth] is the depth the minimax algorithm will evaluate to in a 
One player game *)

(** [player_color] is the color of the pieces that a player chooses to be during
 a One player game type*)
type player_color = Black | White

(** [command] represents a player command to move a to move their piece to a 
square or quit the game during a one or two player game*)
type command = Move of location_phrase | Quit

exception Malformed
(** Raised when a malformed command is encountered. *)

val parse_engine_depth : string -> int
(** [parse_player_depth] parses a player's input into a [depth] in One 
Player game mode.
Raises: [Malformed] if the command is malformed. *)

val parse_player_color : string -> player_color
(** [parse_player_color] parses a player's input into a [player_color] in One 
Player game mode.
Raises: [Malformed] if the command is malformed. *)

val parse_player_command : string -> game_type_command
(** [parse_player_command] parses a player's input into a [game_type_command] in
 One or Two Player game mode.
Raises: [Malformed] if the command is malformed. *)

val parse_move : string -> command
(** [parse_move] parses a player's input into a [command] in One or Two Player 
game mode.
Raises: [Malformed] if the command is malformed. *)
