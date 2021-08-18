open Piece

module State = struct
  type state = {
    turn : Piece.c;
    inCheckwhite : bool;
    inCheckblack : bool;
    stalematewhite : bool;
    stalemateblack : bool;
    checkmatewhite : bool;
    checkmateblack : bool;
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
      inCheckwhite = st.inCheckwhite;
      inCheckblack = st.inCheckblack;
      stalematewhite = st.stalematewhite;
      stalemateblack = st.stalemateblack;
      checkmatewhite = st.checkmatewhite;
      checkmateblack = st.checkmateblack;
      white_has_captured = Array.copy st.white_has_captured;
      black_has_captured = Array.copy st.black_has_captured;
    }
end
