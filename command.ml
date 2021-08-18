type location_phrase = string array

type game_type_command = One | Two | Q | Sim | Sim2 of int

type depth = int

(** [player_color] is the color of the pieces that a player chooses to be during
 a One player game type*)
type player_color = Black | White

type command = Move of location_phrase | Quit

exception Malformed
(** Raised when a malformed command is encountered. *)

(** [parse_player_depth] parses a player's input into a [depth] in One 
Player game mode.
Raises: [Malformed] if the command is malformed. *)
let parse_engine_depth str =
  let lst = List.filter (( <> ) "") (String.split_on_char ' ' str) in
  match lst with
  | [] -> raise Malformed
  | [ a ] -> (
      try
        let i_a = int_of_string a in
        if i_a > 0 then i_a else raise Malformed
      with _ -> raise Malformed)
  | _ -> raise Malformed

(** [parse_player_color] parses a player's input into a [player_color] in One 
Player game mode.
Raises: [Malformed] if the command is malformed. *)
let parse_player_color str =
  let lst = List.filter (( <> ) "") (String.split_on_char ' ' str) in
  match lst with
  | [] -> raise Malformed
  | [ a ] ->
      if a = "Black" then Black
      else if a = "black" then Black
      else if a = "White" then White
      else if a = "white" then White
      else raise Malformed
  | _ -> raise Malformed

(** [parse_player_command] parses a player's input into a [game_type_command] in
 One or Two Player game mode.
Raises: [Malformed] if the command is malformed. *)
let parse_player_command str =
  let lst = List.filter (( <> ) "") (String.split_on_char ' ' str) in
  match lst with
  | [] -> raise Malformed
  | [ a ] ->
      if a = "quit" then Q
      else if a = "1" then One
      else if a = "2" then Two
      else if a = "spectate" then Sim
      else raise Malformed
  | [ a; b ] -> (
      try
        let i_b = int_of_string b in
        if a = "spectate" && i_b > 0 then Sim2 i_b else raise Malformed
      with _ -> raise Malformed)
  | _ -> raise Malformed

(** [parse_move] parses a player's input into a [command] in One or Two Player 
game mode.
Raises: [Malformed] if the command is malformed. *)
let parse_move str =
  let lst = List.filter (( <> ) "") (String.split_on_char ' ' str) in
  match lst with
  | [] -> raise Malformed
  | [ a ] -> if a = "quit" then Quit else raise Malformed
  | [ a; b; c ] ->
      if b = "to" then
        if String.length a = 2 && String.length c = 2 then Move [| a; c |]
        else raise Malformed
      else raise Malformed
  | _ -> raise Malformed
