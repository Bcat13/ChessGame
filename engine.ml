open Piece
open Square
open Board
open State

(** [loc_to_square] is the square that the piece with location [loc] is on in a 
specific [board] *)
let loc_to_square (board : Board.b) (loc : Piece.l) =
  let index1 = Piece.loc_to_arr_index loc in
  board.tiles.(index1.(0)).(index1.(1))

(* [get_next_moves_piece] is a list of moves for a piece on [square1] in a
   [board]. A move has type Square.square * Square.square *)
let rec get_next_moves_piece (board : Board.b) (square1 : Square.square)
    (next_move_locs : Piece.n_m) acc =
  match next_move_locs with
  | [] -> acc
  | h :: t ->
      get_next_moves_piece board square1 t
        ((square1, loc_to_square board h) :: acc)

(** [get_next_moves board piece_list acc] is a list *)
let rec get_next_moves (board : Board.b) (piece_list : Piece.piece list)
    (acc : (Square.square * Square.square) list) =
  match piece_list with
  | [] -> acc
  | h :: t ->
      let square1 = loc_to_square board h.location in
      get_next_moves board t
        (get_next_moves_piece board square1 h.next_moves [] @ acc)

(** [move_to_str_arr] converts a move of type Square.square * Square.square to
the corresponding Command.location_phrase *)
let move_to_str_arr (move : Square.square * Square.square) =
  let first = fst move in
  let r1 = string_of_int first.rank in
  let c1 = Char.escaped first.file in
  let second = snd move in
  let r2 = string_of_int second.rank in
  let c2 = Char.escaped second.file in
  [| c1 ^ r1; c2 ^ r2 |]

(** [max_tuple] is the tuple whose first element is larger*)
let max_tuple t1 t2 = if fst t1 >= fst t2 then t1 else t2

(** [min_tuple] is the tuple whose first element is smaller*)
let min_tuple t1 t2 = if fst t1 <= fst t2 then t1 else t2

(** [evaluate] is a integer ranking the position of particular [board]
in a particular [state] *)
let evaluate board (state : State.state) =
  let score = Board.get_game_score board in
  let turn = if state.turn = White then -1 else 1 in
  if state.checkmate_black || state.checkmate_white then 99999 * turn
  else if state.stalemate_white || state.stalemate_black then 0
  else score

(** [pp_int_array] prints an int array *)
let pp_int_array arr : unit =
  for x = 0 to Array.length arr - 1 do
    print_string (string_of_int arr.(x) ^ ",")
  done;
  print_endline ""

let rec minimax board (state : State.state) depth rec_move is_first =
  let maximizing_player = state.turn = White in
  let game_over =
    state.stalemate_white || state.stalemate_black || state.checkmate_white
    || state.checkmate_black
  in
  if depth = 0 || game_over then (evaluate board state, rec_move)
  else
    let board_copy = Board.copy_board board in
    if maximizing_player then (
      let max_eval = ref (Int.min_int, rec_move) in
      let pl =
        List.filter
          (fun (x : Piece.piece) -> x.color = state.turn)
          board_copy.piece_list
      in
      let move_list = Array.of_list (get_next_moves board pl []) in
      let length = Array.length move_list in
      for i = 0 to length - 1 do
        let bc = Board.copy_board board in
        let move = move_list.(i) in
        let move_str_arr = move_to_str_arr move in
        let square1 = Board.get_square bc move_str_arr.(0) in
        let square2 = Board.get_square bc move_str_arr.(1) in
        let square2_occ = square2.occupant in
        let recursive_move = if is_first then move_list.(i) else rec_move in
        Board.move bc square1 square2 state true;
        let sc = State.state_copy state in
        let new_state = sc |> Board.update_state bc square2_occ in
        let eval = minimax bc new_state (depth - 1) recursive_move false in
        max_eval := max_tuple eval !max_eval
      done;
      !max_eval)
    else
      let min_eval = ref (Int.max_int, rec_move) in
      let pl =
        List.filter
          (fun (x : Piece.piece) -> x.color = state.turn)
          board_copy.piece_list
      in
      let move_list = Array.of_list (get_next_moves board pl []) in
      let length = Array.length move_list in
      for i = 0 to length - 1 do
        let bc = Board.copy_board board in
        let move = move_list.(i) in
        let move_str_arr = move_to_str_arr move in
        let square1 = Board.get_square bc move_str_arr.(0) in
        let square2 = Board.get_square bc move_str_arr.(1) in
        let square2_occ = square2.occupant in
        let recursive_move = if is_first then move_list.(i) else rec_move in
        Board.move bc square1 square2 state true;
        let sc = State.state_copy state in
        let new_state = sc |> Board.update_state bc square2_occ in
        let eval = minimax bc new_state (depth - 1) recursive_move false in
        min_eval := min_tuple eval !min_eval
      done;
      !min_eval
