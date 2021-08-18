(** [Piece] is a piece on a chess board*)

(** [k] is a kind of chess piece: Pawn, Kinght, Bishop, Rook, 
  Queen, King *)
type k = Pawn | Knight | Bishop | Rook | Queen | King

(** [c] is a color of a chess piece *)
type c = White | Black

type w = int
(** [w] is the worth of a chess piece *)

type l = char * int
(** [l] is the location of a piece on a chess board. AF: the tuple 
  char * int represents the location <file><rank> on an 8x8 chess board *)

type n_m = l list
(** [n_m] is the list of next possible locations [l] for a piece to move to.*)

type piece = {
  name : k;
  color : c;
  worth : w;
  mutable location : l;
  mutable next_moves : n_m;
  mutable num_moves : int;
}
(** [piece] is the type for piece*)

val loc_to_arr_index : l -> int array
(** [loc_to_arr_index location] is the index in a two-dimentional array given
  a [location]*)

val copy_piece : piece -> piece
(** [copy_piece p] is the physical copy of a piece *)

val pp_print_loc : char * int -> unit
(** [pp_print_loc l] prints a location [l]*)

val pp_type : k -> unit
(** [pp_type k] prints the type [k] of a piece*)

val pp_piece : piece -> unit
(** [pp_piece p] prints the type and location of a piece [p]*)

val pp_pl : piece list -> unit
(** [pp_pl pl] prints the type and location of pieces in a piecelist [pl]*)
