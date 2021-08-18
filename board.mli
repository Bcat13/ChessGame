(** [Board] is a chess board with a list of pieces*)

type b = {
  tiles : Square.square array array;
  mutable piece_list : Piece.piece list;
}
(** [b] is the type for a board *)

exception InvalidMove
(** Raised when a move is made that is not allowed in chess. *)

exception InvalidSquare
(** Raised when a move is made to or from an square not in the chess board. *)

exception EmptySquare
(** Raised when a square is expected to have a occupant Some t but instead 
  has occupant None *)

val update_piece_list : b -> Square.square -> Square.square -> bool -> unit
(**[update_piece_list] updates the piecelist of a board [b] by moving the piece 
  on [square1] to [square2], and possibly removing a piece on [square2] if it 
  exists*)

val update_tiles : b -> Square.square -> Square.square -> bool -> unit
(** [update_tiles board square1 square2 is_ep] updates the tiles of a board 
  by making the occupant on [square2] be the occupant of [suqare1], and removing
  the occupant on [square1]. If [is_ep] is true then it also removes the pawn on 
  the square that was taken in en_passant*)

val in_check : b -> Piece.c -> bool
(** [inCheck board turn] is true if the King whose [turn] it is happens to be 
  on the [board] in check and false otherwise.
  Requires: The kings of both colors exist in the piece_list of [board] *)

val update_next_moves : b -> unit
(** [update_next_moves board] updates each pieces next moves list on the 
  [board]*)

val get_square : b -> string -> Square.square
(** [get_square board str] is the square on the [board] represented by the 
  string [str]
  Raises: [InvalidSquare] if the string is not a represented as a <file><rank> 
    on a chess board*)

val copy_board : b -> b
(** [copy_board board] is a physical copy of [board]*)

val get_game_score : b -> int
(** [get_game_score state board] is the integer score used in normal chess 
  that is derived from the number and worth of the black and white chess pieces 
  on the [board] at that moment in time*)

val move : b -> Square.square -> Square.square -> State.state -> bool -> unit
(** [move board square1 square2 state is_engine] mutates a [board] with a 
  current [state] by moving a piece on [square1] to [square2]. If [is_engine]
  is true and the move is a pawn promotion then it auto promotes to a queen*)

val update_state : b -> Square.o -> State.state -> State.state
(** [update_state board sq2_occupant state] is the new state with 
  [sq2_occupant] added to the list of captured pieces after making a move on 
  the [board] which was in [state]*)

(****************************************************************************)
(*************************HELPERS TO TEST???????*****************************)
(****************************************************************************)
val in_stalemate : b -> Piece.c -> bool

val get_piece : Piece.piece option -> Piece.piece

val path_for_piece : b -> Piece.piece -> int -> int -> (char * int) list

val square_for_piece : b -> Piece.piece -> int -> int -> (char * int) list

val paths_for_pawn : b -> Piece.piece -> int -> int -> bool -> (char * int) list

val get_arr_index : Piece.piece -> int array
