open Piece

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

(** [string_of_turn] is the string representation of the current turn *)
let string_of_turn (turn : Piece.c) : string =
  if turn = Black then "Black" else "White"

(** [state_copy] is a deep copy of the current state *)
let state_copy st =
  {
    turn = st.turn;
    in_check_white = st.in_check_white;
    in_check_black = st.in_check_black;
    stalemate_white = st.stalemate_white;
    stalemate_black = st.stalemate_black;
    checkmate_white = st.checkmate_white;
    checkmate_black = st.checkmate_black;
    white_has_captured = Array.copy st.white_has_captured;
    black_has_captured = Array.copy st.black_has_captured;
  }
