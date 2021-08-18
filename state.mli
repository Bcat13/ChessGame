(** [State] keeps track of the state of the game *)

type state = {
  turn : Piece.c;
  in_check_white : bool;
  in_check_black : bool;
  stalemate_white : bool;
  stalemate_black : bool;
  checkmate_white : bool;
  checkmate_black : bool;
  white_has_captured : int array;
  black_has_captured : int array;
}
(** [state] is the current state of the game, tracking checks, statelamtes, 
  and pieces captured. *)

val string_of_turn : Piece.c -> string
(** [string_of_turn] is the string representation of the current turn *)

val state_copy : state -> state
(** [state_copy] is a deep copy of the current state *)
