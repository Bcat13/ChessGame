(** Initializes board and the state*)

val piecelist : Piece.piece list
(** [piecelist] is the initial list of pieces in a game of chess*)

val board : Square.square array array
(** [board] is the initial set of squares in a game of chess*)

val state : State.state
(** [board] is the initial state in a game of chess*)
