(** [Engine] is a module for the engine functions*)
module Engine : sig
  val get_next_moves :
    Board.Board.b ->
    Piece.Piece.piece list ->
    (Square.Square.square * Square.Square.square) list ->
    (Square.Square.square * Square.Square.square) list
  (** [get_next_moves board piece_list acc] is a list of all the possible moves 
  for the pieces in a specefied [piece_list] on a [board] with an accumulator 
  [acc]. A move is represented as having type Square.square * Square.square *)

  val minimax :
    Board.Board.b ->
    State.State.state ->
    int ->
    Square.Square.square * Square.Square.square ->
    bool ->
    int * (Square.Square.square * Square.Square.square)
  (** [minimax board state depth rec_move is_first] is a move represented as a 
  Square.Square.square * Square.Square.square on particular [board] in a 
  particular [state], at specefied [depth], with a move from the first recursive 
  call to minimax [rec_move], and a boolean flag [is_first] which is true on the
  first call to minimax*)
end
