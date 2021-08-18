(** [State] keeps track of the state of the game *)
module State : sig
  type state = {
    turn : Piece.Piece.c;
    inCheckwhite : bool;
    inCheckblack : bool;
    stalematewhite : bool;
    stalemateblack : bool;
    checkmatewhite : bool;
    checkmateblack : bool;
    white_has_captured : int array;
    black_has_captured : int array;
  }
  (** [state] is the current state of the game, tracking checks, statelamtes, 
  and pieces captured. *)

  val string_of_turn : Piece.Piece.c -> string
  (** [string_of_turn] is the string representation of the current turn *)

  val state_copy : state -> state
  (** [state_copy] is a deep copy of the current state *)
end
