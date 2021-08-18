open Piece

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

(** [empty] is the empty square *)
let empty = { file = ' '; rank = 0; occupant = None; color = White }

(** [copy_square] is the physical copy of a square *)
let copy_square (sq : square) =
  let new_occ =
    match sq.occupant with None -> None | Some x -> Some (Piece.copy_piece x)
  in
  { file = sq.file; rank = sq.rank; occupant = new_occ; color = sq.color }

(** [rank_to_index] is the row index in an 8x8 array given the [rank] of a 
  square in 8x8 chess board *)
let rank_to_index (rank : r) : int = 8 - rank

(** [file_to_index] is the column index in an 8x8 array given the [file] of a 
  square in 8x8 chess board *)
let file_to_index (file : f) : int = Char.code file - 97

(** [pp_occupant] prints the occupant of a [square]*)
let pp_occupant square =
  match square.occupant with
  | None -> print_string "None"
  | Some t -> Piece.pp_type t.name

(** [pp_square] prints the file, rank, and occupant of a [square] *)
let pp_square square =
  print_string "file: ";
  print_string (Char.escaped square.file);
  print_string " rank: ";
  print_string (string_of_int square.rank);
  print_string " occ: ";
  pp_occupant square;
  print_endline ""
