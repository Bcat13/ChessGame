(** [Square] is a singular square tile on a chess board*)

type f = char
(** [f] is the file of a square on a chess board*)

type r = int
(** [r] is the rank of a square on a chess board*)

type o = Piece.piece option
(** [o] is the occupant of a square on a chess board. An occupant is either 
  None or Some piece *)

type square = { file : f; rank : r; mutable occupant : o; color : Piece.c }
(** [square] is the type for a square*)

val empty : square
(** [empty] is the empty square *)

val copy_square : square -> square
(** [copy_square sq] is the physical copy of a square *)

val rank_to_index : r -> int
(** [rank_to_index rank] is the row index in an 8x8 array given the [rank] of a 
  square in 8x8 chess board *)

val file_to_index : f -> int
(** [file_to_index file] is the column index in an 8x8 array given the [file] of a 
  square in 8x8 chess board *)

val pp_occupant : square -> unit
(** [pp_occupant square] prints the occupant of a [square]*)

val pp_square : square -> unit
(** [pp_square square] prints the file, rank, and occupant of a [square] *)
