(** [Engine] is a module for the engine functions*)

val get_next_moves :
  Board.b ->
  Piece.piece list ->
  (Square.square * Square.square) list ->
  (Square.square * Square.square) list
(** [get_next_moves board piece_list acc] is a list of all the possible moves 
  for the pieces in a specefied [piece_list] on a [board] with an accumulator 
  [acc]. A move is represented as having type Square.square * Square.square *)

val minimax :
  Board.b ->
  State.state ->
  int ->
  Square.square * Square.square ->
  bool ->
  int * (Square.square * Square.square)
(** [minimax board state depth rec_move is_first] is a move represented as a 
  Square.Square.square * Square.Square.square on particular [board] in a 
  particular [state], at specefied [depth], with a move from the first recursive 
  call to minimax [rec_move], and a boolean flag [is_first] which is true on the
  first call to minimax*)
