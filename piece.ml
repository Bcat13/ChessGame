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

(** [loc_to_arr_index location] is the index in a two-dimentional array given
  a [location]*)
let loc_to_arr_index (location : l) =
  [| 8 - snd location; (fst location |> Char.code) - 97 |]

(** [copy_piece] is the physical copy of a piece *)
let copy_piece (p : piece) =
  {
    name = p.name;
    color = p.color;
    worth = p.worth;
    location = p.location;
    next_moves = p.next_moves;
    num_moves = p.num_moves;
  }

(** [pp_print_loc] prints a location [l]*)
let pp_print_loc l =
  print_string (Char.escaped (fst l));
  print_endline (string_of_int (snd l))

(** [pp_type] prints the type [k] of a piece*)
let pp_type k =
  match k with
  | Pawn -> print_string " pawn "
  | Knight -> print_string " knight "
  | Bishop -> print_string " bishop "
  | Rook -> print_string " rook "
  | Queen -> print_string " queen "
  | King -> print_string " king "

(** [pp_piece] prints the type and location of a piece [p]*)
let pp_piece p =
  pp_type p.name;
  pp_print_loc p.location

(** [pp_pl] prints the type and location of pieces in a piecelist [pl]*)
let pp_pl pl = List.iter pp_piece pl
