open Piece
open Square
open State

module Board = struct
  type b = {
    tiles : Square.square array array;
    mutable piece_list : Piece.piece list;
  }

  exception InvalidMove

  exception InvalidSquare

  exception EmptySquare

  (** [get_piece] is the piece from a piece option [piece_opt]. This is 
  typically used to find the occupant on a square*)
  let get_piece (piece_opt : Piece.piece option) =
    match piece_opt with Some t -> t | None -> raise EmptySquare

  (** [copy_tiles] is physical copy of the [tiles] in a board*)
  let copy_tiles (tiles : Square.square array array) : Square.square array array
      =
    Array.map (fun x -> Array.map Square.copy_square x) tiles

  (** [tiles_flatten] is a flattened two-dimentional array [arr]. 
  Requires: arr is a two-dimensional 8x8 array*)
  let tiles_flatten arr =
    Array.concat
      [ arr.(0); arr.(1); arr.(2); arr.(3); arr.(4); arr.(5); arr.(6); arr.(7) ]

  (** [copy_piece_list] is the list of pieces that are occupants of squares in 
  [tiles] *)
  let copy_piece_list (tiles : Square.square array array) : Piece.piece list =
    tiles_flatten tiles |> Array.to_list
    |> List.filter (fun (x : Square.square) -> x.occupant <> None)
    |> List.map (fun (y : Square.square) -> get_piece y.occupant)

  (** [copy_board] creates a physical copy of *)
  let copy_board (board : b) =
    let tiles_copy = copy_tiles board.tiles in
    let piece_list_copy = copy_piece_list tiles_copy in
    { tiles = tiles_copy; piece_list = piece_list_copy }

  (**[get_arr_index] is the row-column [][] array index of a piece on a 
  particular file and rank of the board.*)
  let get_arr_index (piece : Piece.piece) =
    [| 8 - snd piece.location; (fst piece.location |> Char.code) - 97 |]

  let get_file_rank (index : int array) =
    (Char.chr (index.(1) + 97), 8 - index.(0))

  (**[is_valid_move] checks if a move by a piece on [square1] to another 
      [square2] on the board is valid. 
      Requires: there is a valid piece on [square1] *)
  let is_valid_move (square1 : Square.square) (square2 : Square.square)
      (turn : Piece.c) =
    let piece = get_piece square1.occupant in
    if piece.color <> turn then false
    else
      let square2_location = (square2.file, square2.rank) in
      List.exists (( = ) square2_location) piece.next_moves

  let square_in_tiles sq tiles =
    let b = ref false in
    for i = 0 to 7 do
      if Array.memq sq tiles.(i) then b := true else ()
    done;
    !b

  (**[updatepieceList] updates the piecelist of a board [b] by moving the piece 
  on [square1] to [square2], and possibly removing a piece on [square2] if it 
  exists*)
  let updatepieceList (board : b) (square1 : Square.square)
      (square2 : Square.square) (is_ep : bool) =
    let pl = board.piece_list in
    let piece1 = get_piece square1.occupant in
    (* move piece from square1 to square2 **)
    piece1.location <- (square2.file, square2.rank);
    (*increment the number of times this piece has moved by 1*)
    piece1.num_moves <- piece1.num_moves + 1;

    (* remove piece on square2 from piecelist**)
    if square2.occupant <> None then
      board.piece_list <-
        List.filter (fun x -> x <> get_piece square2.occupant) pl
    else if is_ep then
      let s3_row = Square.rank_to_index square1.rank in
      let s3_col = Square.file_to_index square2.file in
      let square3 = board.tiles.(s3_row).(s3_col) in
      board.piece_list <-
        List.filter (fun x -> x <> get_piece square3.occupant) pl
    else ()

  let reverse_update_piece_list (old_location : Piece.l)
      (old_pl : Piece.piece list) (board : b) (square1 : Square.square) =
    (* If piece was taken on [square2] put it back*)
    board.piece_list <- old_pl;

    (* sets piece location, currently the occupant of [square1], (because
       [reverse_update_tiles] has already been called) to the location of
       [square1]*)
    let piece1 = get_piece square1.occupant in
    piece1.location <- old_location;
    (*decrement the number of times this piece has moved by 1*)
    piece1.num_moves <- piece1.num_moves - 1

  (** [reverse_update_tiles] updates the tiles of a board by moving the piece
  on [square2] back to [square1] and possibly adding the old occupant [old] 
  to [square2]*)
  let reverse_update_tiles (board : b) (old : Square.o)
      (square1 : Square.square) (square2 : Square.square) (is_ep : bool) =
    square1.occupant <- square2.occupant;
    if is_ep then (
      let s3_row = Square.rank_to_index square1.rank in
      let s3_col = Square.file_to_index square2.file in
      let square3 = board.tiles.(s3_row).(s3_col) in
      square2.occupant <- None;
      square3.occupant <- old)
    else square2.occupant <- old

  let update_tiles (board : b) (square1 : Square.square)
      (square2 : Square.square) (is_ep : bool) =
    square2.occupant <- square1.occupant;
    square1.occupant <- None;
    if is_ep then
      let s3_row = Square.rank_to_index square1.rank in
      let s3_col = Square.file_to_index square2.file in
      let square3 = board.tiles.(s3_row).(s3_col) in
      square3.occupant <- None

  (**[in_bounds] is true if the index [x][y] is a valid index in the array
  representation of an 8X8 chess board and false otherwise*)
  let in_bounds (x : int) (y : int) : bool = x >= 0 && x < 8 && y >= 0 && y < 8

  let get_square (board : b) str =
    let file = Square.file_to_index str.[0] in
    let rank = Char.code str.[1] - 48 |> Square.rank_to_index in
    if in_bounds file rank then board.tiles.(rank).(file)
    else raise InvalidSquare

  (**[ic_by_line] checks if a king is put in check by an object on a straight or
  diagonal line. Does not include Pawns. *)
  let rec ic_by_line (tiles : Square.square array array) (row : int) (col : int)
      (x : int) (y : int) (turn : Piece.c) (piece_type : Piece.k) : bool =
    if in_bounds (row + y) (col + x) then
      let square_to_check = tiles.(row + y).(col + x) in
      match square_to_check.occupant with
      | None -> ic_by_line tiles (row + y) (col + x) x y turn piece_type
      | Some x ->
          if x.color = turn then false
          else if x.name = Queen || x.name = piece_type then true
          else false
    else false

  (** [in_check_by_pawn] checks if the king is put in check by a pawn*)
  let ic_by_pawn2 (tiles : Square.square array array) (row : int) (col : int)
      (x : int) (turn : Piece.c) : bool =
    let y = if turn = White then -1 else 1 in
    if in_bounds (row + y) (col + x) then
      let square_to_check = tiles.(row + y).(col + x) in
      match square_to_check.occupant with
      | None -> false
      | Some x ->
          if x.color = turn then false
          else if x.name = Pawn then true
          else false
    else false

  (***[in_check_by_knight] checks if the king is put in check by a knight. *)
  let ic_by_knight (tiles : Square.square array array) (row : int) (col : int)
      (x : int) (y : int) (turn : Piece.c) : bool =
    if in_bounds (row + y) (col + x) then
      match tiles.(row + y).(col + x).occupant with
      | None -> false
      | Some x ->
          if x.color = turn then false
          else if x.name = Knight then true
          else false
    else false

  (***[in_check_by_king] checks if the king would be put in check by the other
    players king. *)
  let ic_by_king (tiles : Square.square array array) (row : int) (col : int)
      (x : int) (y : int) (turn : Piece.c) : bool =
    if in_bounds (row + y) (col + x) then
      match tiles.(row + y).(col + x).occupant with
      | None -> false
      | Some x ->
          if x.color = turn then false
          else if x.name = King then true
          else false
    else false

  (** [inCheck] is true if the King whose turn it is happens to be in check and 
  false otherwise.
  Requires: kings of both colors exist in the piece list of [board] *)
  let in_check (board : b) (turn : Piece.c) : bool =
    let pl = board.piece_list in
    let king =
      List.find (fun (x : Piece.piece) -> x.name = King && x.color = turn) pl
    in
    let king_row = (get_arr_index king).(0) in
    let king_col = (get_arr_index king).(1) in
    (*West, East, South, North*)
    ic_by_line board.tiles king_row king_col (-1) 0 turn Piece.Rook
    || ic_by_line board.tiles king_row king_col 1 0 turn Piece.Rook
    || ic_by_line board.tiles king_row king_col 0 1 turn Piece.Rook
    || ic_by_line board.tiles king_row king_col 0 (-1) turn Piece.Rook
    (*SW, SE, NE, NW*)
    || ic_by_line board.tiles king_row king_col (-1) 1 turn Piece.Bishop
    || ic_by_line board.tiles king_row king_col 1 1 turn Piece.Bishop
    || ic_by_line board.tiles king_row king_col 1 (-1) turn Piece.Bishop
    || ic_by_line board.tiles king_row king_col (-1) (-1) turn Piece.Bishop
    (*By a Pawn*)
    || ic_by_pawn2 board.tiles king_row king_col 1 turn
    || ic_by_pawn2 board.tiles king_row king_col (-1) turn
    (*8 Knight Positions*)
    || ic_by_knight board.tiles king_row king_col (-2) 1 turn
    || ic_by_knight board.tiles king_row king_col (-1) 2 turn
    || ic_by_knight board.tiles king_row king_col 1 2 turn
    || ic_by_knight board.tiles king_row king_col 2 1 turn
    || ic_by_knight board.tiles king_row king_col 2 (-1) turn
    || ic_by_knight board.tiles king_row king_col 1 (-2) turn
    || ic_by_knight board.tiles king_row king_col (-1) (-2) turn
    || ic_by_knight board.tiles king_row king_col (-2) (-1) turn
    (*8 King Positions*)
    || ic_by_king board.tiles king_row king_col 0 1 turn
    || ic_by_king board.tiles king_row king_col 0 (-1) turn
    || ic_by_king board.tiles king_row king_col 1 0 turn
    || ic_by_king board.tiles king_row king_col 1 1 turn
    || ic_by_king board.tiles king_row king_col 1 (-1) turn
    || ic_by_king board.tiles king_row king_col (-1) 0 turn
    || ic_by_king board.tiles king_row king_col (-1) 1 turn
    || ic_by_king board.tiles king_row king_col (-1) (-1) turn

  let paths_for_pawn (board : b) (piece : Piece.piece) (c_incr : int)
      (r_incr : int) (is_take : bool) =
    let (row : int) = (get_arr_index piece).(0) in
    let (col : int) = (get_arr_index piece).(1) in
    if in_bounds (row + r_incr) (col + c_incr) then
      if is_take then
        let square_to_check = board.tiles.(row + r_incr).(col + c_incr) in
        if square_to_check.occupant <> None then
          let occ = get_piece square_to_check.occupant in
          if occ.color <> piece.color then
            [ get_file_rank [| row + r_incr; col + c_incr |] ]
          else []
        else []
      else if r_incr = 2 || r_incr = -2 then
        let square_to_check1 =
          board.tiles.(row + r_incr).(col + (c_incr / 2))
        in
        let square_to_check2 = board.tiles.(row + r_incr).(col + c_incr) in
        if square_to_check1.occupant = None && square_to_check2.occupant = None
        then [ get_file_rank [| row + r_incr; col + c_incr |] ]
        else []
      else
        let square_to_check = board.tiles.(row + r_incr).(col + c_incr) in
        if square_to_check.occupant = None then
          [ get_file_rank [| row + r_incr; col + c_incr |] ]
        else []
    else []

  (** [pawn_next_moves] is the list of squares to which the pawn can move*)
  let pawn_next_moves (board : b) (piece : Piece.piece) =
    (*let left = paths_for_pawn board piece 0 (-1) in*)
    let color_int = if piece.color = White then -1 else 1 in
    let takes_left = paths_for_pawn board piece color_int color_int true in
    let moves_1 = paths_for_pawn board piece 0 color_int false in
    let moves_2 = paths_for_pawn board piece 0 (2 * color_int) false in
    let takes_right =
      paths_for_pawn board piece (-1 * color_int) color_int true
    in
    (*let right = paths_for_pawn b piece 0 1 in*)
    if piece.num_moves = 0 && moves_1 <> [] then
      takes_left @ moves_1 @ moves_2 @ takes_right
    else takes_left @ moves_1 @ takes_right

  let next_incr (number : int) : int =
    if number > 0 then number + 1 else if number < 0 then number + -1 else 0

  let rec path_for_piece (board : b) (piece : Piece.piece) (c_incr : int)
      (r_incr : int) =
    let (row : int) = (get_arr_index piece).(0) in
    let (col : int) = (get_arr_index piece).(1) in
    let color = piece.color in
    if in_bounds (row + r_incr) (col + c_incr) then
      let square_check = board.tiles.(row + r_incr).(col + c_incr) in
      match square_check.occupant with
      | None ->
          [ get_file_rank [| row + r_incr; col + c_incr |] ]
          @ path_for_piece board piece (next_incr c_incr) (next_incr r_incr)
      | Some t ->
          if color <> t.color then
            [ get_file_rank [| row + r_incr; col + c_incr |] ]
          else []
    else []

  (** [rook_next_moves] is the list of squares to which the rook can move*)
  let rook_next_moves (board : b) (piece : Piece.piece) =
    let color_int = if piece.color = White then -1 else 1 in
    let move_forward = path_for_piece board piece 0 color_int in
    let move_backward = path_for_piece board piece 0 (-1 * color_int) in
    let move_left = path_for_piece board piece color_int 0 in
    let move_right = path_for_piece board piece (-1 * color_int) 0 in
    move_forward @ move_backward @ move_left @ move_right

  let bishop_next_moves (board : b) (piece : Piece.piece) =
    let color_int = if piece.color = White then -1 else 1 in
    let move_fl = path_for_piece board piece color_int color_int in
    let move_fr = path_for_piece board piece (-1 * color_int) color_int in
    let move_bl = path_for_piece board piece color_int (-1 * color_int) in
    let move_br =
      path_for_piece board piece (-1 * color_int) (-1 * color_int)
    in
    move_fl @ move_fr @ move_bl @ move_br

  let queen_next_moves (board : b) (piece : Piece.piece) =
    bishop_next_moves board piece @ rook_next_moves board piece

  let square_for_piece (board : b) (piece : Piece.piece) (c_incr : int)
      (r_incr : int) =
    let (row : int) = (get_arr_index piece).(0) in
    let (col : int) = (get_arr_index piece).(1) in
    let color = piece.color in
    if in_bounds (row + r_incr) (col + c_incr) then
      let square_check = board.tiles.(row + r_incr).(col + c_incr) in
      match square_check.occupant with
      | None -> [ get_file_rank [| row + r_incr; col + c_incr |] ]
      | Some t ->
          if color <> t.color then
            [ get_file_rank [| row + r_incr; col + c_incr |] ]
          else []
    else []

  let knight_next_moves (board : b) (piece : Piece.piece) =
    let one = square_for_piece board piece (-2) 1 in
    let two = square_for_piece board piece (-1) 2 in
    let three = square_for_piece board piece 1 2 in
    let four = square_for_piece board piece 2 1 in
    let five = square_for_piece board piece 2 (-1) in
    let six = square_for_piece board piece 1 (-2) in
    let seven = square_for_piece board piece (-1) (-2) in
    let eight = square_for_piece board piece (-2) (-1) in
    one @ two @ three @ four @ five @ six @ seven @ eight

  let king_next_moves (board : b) (piece : Piece.piece) =
    let one = square_for_piece board piece 0 (-1) in
    let two = square_for_piece board piece 1 (-1) in
    let three = square_for_piece board piece 1 0 in
    let four = square_for_piece board piece 1 1 in
    let five = square_for_piece board piece 0 1 in
    let six = square_for_piece board piece (-1) 1 in
    let seven = square_for_piece board piece (-1) 0 in
    let eight = square_for_piece board piece (-1) (-1) in
    one @ two @ three @ four @ five @ six @ seven @ eight

  let dnr_in_check (board : b) (piece1 : Piece.piece) (turn : Piece.c)
      (loc2 : Piece.l) : bool =
    let index1 = Piece.loc_to_arr_index piece1.location in
    let index2 = Piece.loc_to_arr_index loc2 in
    let square1 = board.tiles.(index1.(0)).(index1.(1)) in
    let square2 = board.tiles.(index2.(0)).(index2.(1)) in
    let old_piece_list = board.piece_list in
    let old_square2_occupant = square2.occupant in
    let old_square1_location = piece1.location in
    updatepieceList board square1 square2 false;
    update_tiles board square1 square2 false;
    let ic = in_check board turn in
    reverse_update_tiles board old_square2_occupant square1 square2 false;
    reverse_update_piece_list old_square1_location old_piece_list board square1;
    not ic

  let dnr_in_check_ep (board : b) (piece1 : Piece.piece) (turn : Piece.c)
      (loc2 : Piece.l) : bool =
    let incr = if turn = Black then 1 else -1 in
    let index1 = Piece.loc_to_arr_index piece1.location in
    let index3 = Piece.loc_to_arr_index loc2 in
    let square1 = board.tiles.(index1.(0)).(index1.(1)) in
    let square2 = board.tiles.(index3.(0) + incr).(index3.(1)) in
    let square3 = board.tiles.(index1.(0)).(index3.(1)) in
    let old_piece_list = board.piece_list in
    let old_square3_occupant = square3.occupant in
    let old_square1_location = piece1.location in
    updatepieceList board square1 square2 true;
    update_tiles board square1 square2 true;
    let ic = in_check board turn in
    reverse_update_tiles board old_square3_occupant square1 square2 true;
    reverse_update_piece_list old_square1_location old_piece_list board square1;
    not ic

  let have_moved (board : b) (king : Piece.piece) (king_side : bool) =
    if king.num_moves > 0 then true
    else
      let index1 = get_arr_index king in
      let rook_row = index1.(0) in
      let rook_col = if king_side then index1.(1) + 3 else index1.(1) - 4 in
      let rook_square = board.tiles.(rook_row).(rook_col) in
      if rook_square.occupant = None then true
      else
        let piece = get_piece rook_square.occupant in
        not (piece.name = Rook && piece.num_moves = 0)

  let empty_castle_rank (board : b) (king : Piece.piece) (king_side : bool) =
    let index = get_arr_index king in
    let row = index.(0) in
    let col = index.(1) in
    if king_side then
      board.tiles.(row).(col + 1).occupant = None
      && board.tiles.(row).(col + 2).occupant = None
    else
      board.tiles.(row).(col - 1).occupant = None
      && board.tiles.(row).(col - 2).occupant = None
      && board.tiles.(row).(col - 3).occupant = None

  let can_castle (board : b) (king : Piece.piece) (turn : Piece.c)
      (king_side : bool) : bool =
    let direction = if king_side then 1 else -1 in
    if have_moved board king king_side then false
    else if not (empty_castle_rank board king king_side) then false
    else if in_check board turn then false
    else
      let loc1 =
        ( (fst king.location |> Char.code) + direction |> Char.chr,
          snd king.location )
      in
      let loc2 =
        ( (fst king.location |> Char.code) + (2 * direction) |> Char.chr,
          snd king.location )
      in
      dnr_in_check board king turn loc1 && dnr_in_check board king turn loc2

  let update_next_move (board : b) (piece : Piece.piece) =
    match piece.name with
    | Pawn ->
        piece.next_moves <-
          List.filter
            (dnr_in_check board piece piece.color)
            (pawn_next_moves board piece)
    | Knight ->
        piece.next_moves <-
          List.filter
            (dnr_in_check board piece piece.color)
            (knight_next_moves board piece)
    | Bishop ->
        piece.next_moves <-
          List.filter
            (dnr_in_check board piece piece.color)
            (bishop_next_moves board piece)
    | Rook ->
        piece.next_moves <-
          List.filter
            (dnr_in_check board piece piece.color)
            (rook_next_moves board piece)
    | Queen ->
        piece.next_moves <-
          List.filter
            (dnr_in_check board piece piece.color)
            (queen_next_moves board piece)
    | King ->
        let king_next_mvs = king_next_moves board piece in
        let king_next_mvs1 =
          if can_castle board piece piece.color true then
            ('g', snd piece.location) :: king_next_mvs
          else king_next_mvs
        in
        let king_next_mvs2 =
          if can_castle board piece piece.color false then
            ('c', snd piece.location) :: king_next_mvs1
          else king_next_mvs1
        in
        piece.next_moves <-
          List.filter (dnr_in_check board piece piece.color) king_next_mvs2

  let update_next_moves (board : b) =
    let piece_list = board.piece_list in
    List.iter (update_next_move board) piece_list

  let rec no_valid_moves (lst : Piece.piece list) (turn : Piece.c) =
    match lst with
    | [] -> true
    | h :: t ->
        if h.color = turn then
          if List.length h.next_moves > 0 then false else no_valid_moves t turn
        else no_valid_moves t turn

  let inCheckmate (board : b) (turn : Piece.c) =
    in_check board turn && no_valid_moves board.piece_list turn

  let diff_team_same_sq_color (board : b) (bishop_lst : Piece.piece list) =
    let p1 = List.hd bishop_lst in
    let p2 = List.nth bishop_lst 1 in
    let index1 = Piece.loc_to_arr_index p1.location in
    let index2 = Piece.loc_to_arr_index p2.location in
    let sq1 = board.tiles.(index1.(0)).(index1.(1)) in
    let sq2 = board.tiles.(index2.(0)).(index2.(1)) in
    if p1.color <> p2.color && sq1.color = sq2.color then true else false

  let is_kb_kb_same_color (board : b) =
    let bishop_lst =
      List.filter (fun (x : Piece.piece) -> x.name = Bishop) board.piece_list
    in
    if
      List.length board.piece_list = 4
      && List.length bishop_lst = 2
      && diff_team_same_sq_color board bishop_lst
    then true
    else false

  let is_king_knight_vs_king (board : b) =
    List.length board.piece_list = 3
    && List.exists (fun (x : Piece.piece) -> x.name = Bishop) board.piece_list

  let is_king_bishop_vs_king (board : b) =
    List.length board.piece_list = 3
    && List.exists (fun (x : Piece.piece) -> x.name = Bishop) board.piece_list

  let is_king_vs_king (board : b) = List.length board.piece_list = 2

  let inStalemate (board : b) (turn : Piece.c) =
    ((not (in_check board turn)) && no_valid_moves board.piece_list turn)
    || is_king_vs_king board
    || is_king_bishop_vs_king board
    || is_king_knight_vs_king board
    || is_kb_kb_same_color board

  let update_take (state : State.state) (square2 : Square.o) =
    let get_loc (sq : Piece.k) =
      match sq with
      | Pawn -> 0
      | Knight -> 1
      | Bishop -> 2
      | Rook -> 3
      | Queen -> 4
      | King -> failwith "bad"
    in
    match square2 with
    | Some t ->
        let loc = get_loc t.name in
        if state.turn = Black then
          state.white_has_captured.(loc) <- state.white_has_captured.(loc) + 1
        else
          state.black_has_captured.(loc) <- state.black_has_captured.(loc) + 1
    | None -> ()

  let updateState (board : b) (sq2_occupant : Square.o) (state : State.state) =
    let (new_turn : Piece.c) = if state.turn = White then Black else White in

    let new_inCheckwhite =
      if new_turn = White then in_check board new_turn else state.inCheckwhite
    in
    let new_inCheckblack =
      if new_turn = Black then in_check board new_turn else state.inCheckblack
    in
    let new_stalematewhite =
      if new_turn = White then inStalemate board new_turn
      else state.stalematewhite
    in
    let new_stalemateblack =
      if new_turn = Black then inStalemate board new_turn
      else state.stalemateblack
    in
    let new_checkmatewhite =
      if new_turn = White then inCheckmate board new_turn
      else state.checkmatewhite
    in
    let new_checkmateblack =
      if new_turn = Black then inCheckmate board new_turn
      else state.checkmateblack
    in

    let (st : State.state) =
      {
        turn = new_turn;
        inCheckwhite = new_inCheckwhite;
        inCheckblack = new_inCheckblack;
        stalematewhite = new_stalematewhite;
        stalemateblack = new_stalemateblack;
        checkmatewhite = new_checkmatewhite;
        checkmateblack = new_checkmateblack;
        white_has_captured = state.white_has_captured;
        black_has_captured = state.black_has_captured;
      }
    in
    update_take st sq2_occupant;
    st

  let pp_loc (t : Piece.l) =
    "(" ^ Char.escaped (fst t) ^ ", " ^ string_of_int (snd t) ^ ")"

  let pp_piece_name (t : Piece.k) =
    match t with
    | Pawn -> "Pawn "
    | Knight -> "Knight "
    | Bishop -> "Bishop "
    | Rook -> "Rook "
    | Queen -> "Queen "
    | King -> "King "

  let pp_list pp_elt lst =
    let pp_elts lst =
      let rec loop n acc = function
        | [] -> acc
        | [ h ] -> acc ^ pp_elt h
        | h1 :: (h2 :: t as t') ->
            if n = 100 then acc ^ "..."
            else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
      in
      loop 0 "" lst
    in
    "[" ^ pp_elts lst ^ "]"

  let print_valid_moves (piece : Piece.piece) =
    print_string (pp_piece_name piece.name);
    print_endline (pp_list pp_loc piece.next_moves)

  let print_all_valid_moves (board : b) (state : State.state) =
    let pl = board.piece_list in
    let turn = state.turn in
    let filtered = List.filter (fun (x : Piece.piece) -> x.color = turn) pl in
    List.iter print_valid_moves filtered

  let get_num_moves_piece (p : Piece.piece) = List.length p.next_moves

  let get_num_moves (board : b) (state : State.state) =
    let pl = board.piece_list in
    let turn = state.turn in
    let filtered = List.filter (fun (x : Piece.piece) -> x.color = turn) pl in
    List.fold_left (fun a b -> get_num_moves_piece b + a) 0 filtered

  let pawn_moved_two (square2 : Square.square) =
    let piece = get_piece square2.occupant in
    let rank = snd piece.location in
    if piece.num_moves = 1 && (rank = 4 || rank = 5) && piece.name = Pawn then
      true
    else false

  let give_ep (board : b) (square2 : Square.square) (state : State.state) =
    if pawn_moved_two square2 then (
      let incr = if state.turn = Black then -1 else 1 in
      let tiles = board.tiles in
      let arr_index_loc = get_arr_index (get_piece square2.occupant) in
      let loc = get_file_rank arr_index_loc in
      let row = arr_index_loc.(0) in
      let col = arr_index_loc.(1) in
      let ep_index = get_file_rank [| row + incr; col |] in
      if in_bounds row (col - 1) then
        let left_square = tiles.(row).(col - 1) in
        if
          left_square.occupant <> None
          && (get_piece left_square.occupant).color <> state.turn
        then
          let left_piece = get_piece left_square.occupant in
          if dnr_in_check_ep board left_piece left_piece.color loc then
            left_piece.next_moves <- ep_index :: left_piece.next_moves
          else ()
        else ()
      else ();

      if in_bounds row (col + 1) then
        let right_square = tiles.(row).(col + 1) in
        if
          right_square.occupant <> None
          && (get_piece right_square.occupant).color <> state.turn
        then
          let right_piece = get_piece right_square.occupant in
          if dnr_in_check_ep board right_piece right_piece.color loc then
            right_piece.next_moves <- ep_index :: right_piece.next_moves
          else ()
        else ()
      else ())
    else ()

  let is_diagonal (square1 : Square.square) (square2 : Square.square) : bool =
    let row_diff =
      Square.rank_to_index square1.rank - Square.rank_to_index square2.rank
    in
    let col_diff =
      Square.file_to_index square1.file - Square.file_to_index square2.file
    in
    (row_diff = 1 || row_diff = -1) && (col_diff = 1 || col_diff = -1)

  let is_en_passant (square1 : Square.square) (square2 : Square.square) : bool =
    (get_piece square1.occupant).name = Pawn
    && square2.occupant = None
    && is_diagonal square1 square2

  let is_promotion (square1 : Square.square) : bool =
    let piece = get_piece square1.occupant in
    let r_incr = if piece.color = White then -1 else 1 in
    let new_row = (get_arr_index piece).(0) + r_incr in
    piece.name = Pawn && new_row mod 7 = 0

  let update_take_promote (state : State.state) (new_piece : Piece.piece) =
    let loc =
      match new_piece.name with
      | Knight -> 1
      | Bishop -> 2
      | Rook -> 3
      | Queen -> 4
      | _ -> failwith "bad"
    in
    if state.turn = Black then (
      state.white_has_captured.(loc) <- state.white_has_captured.(loc) - 1;
      state.white_has_captured.(0) <- state.white_has_captured.(0) + 1)
    else (
      state.black_has_captured.(loc) <- state.black_has_captured.(loc) - 1;
      state.black_has_captured.(0) <- state.black_has_captured.(0) + 1)

  let rec promote_pawn (board : b) (state : State.state)
      (square2 : Square.square) (is_engine : bool) =
    let pawn =
      if square2.occupant = None then failwith "Cannot promote empty square"
      else get_piece square2.occupant
    in
    if is_engine then (
      let new_piece : Piece.piece =
        {
          name = Queen;
          color = pawn.color;
          worth = 9;
          location = pawn.location;
          next_moves = [];
          num_moves = 0;
        }
      in
      let pl = board.piece_list in
      square2.occupant <- Some new_piece;
      board.piece_list <- new_piece :: List.filter (fun x -> x <> pawn) pl;
      update_take_promote state new_piece)
    else (
      print_endline
        "Promote Your Pawn (Type 'Q' for Queen, 'R' for Rook, 'N' for Knight, \
         'B' for Bishop)";
      print_string "> ";
      match read_line () with
      | input ->
          if not (input = "Q" || input = "R" || input = "N" || input = "B") then (
            print_endline "Invalid input";
            promote_pawn board state square2 is_engine)
          else
            let new_type : Piece.k =
              if input = "Q" then Queen
              else if input = "R" then Rook
              else if input = "N" then Knight
              else Bishop
            in
            let new_worth =
              if input = "Q" then 9 else if input = "R" then 5 else 3
            in
            let new_piece : Piece.piece =
              {
                name = new_type;
                color = pawn.color;
                worth = new_worth;
                location = pawn.location;
                next_moves = [];
                num_moves = 0;
              }
            in
            let pl = board.piece_list in
            square2.occupant <- Some new_piece;
            board.piece_list <- new_piece :: List.filter (fun x -> x <> pawn) pl;
            update_take_promote state new_piece)

  let rec game_helper (piece_list : Piece.piece list) acc =
    match piece_list with
    | [] -> acc
    | h :: t ->
        if h.color = White then game_helper t (acc + h.worth)
        else game_helper t (acc - h.worth)

  let get_game_score board =
    let piece_list = board.piece_list in
    game_helper piece_list 0

  (* Determines if we are castling*)
  let is_castle (board : b) (square1 : Square.square) (square2 : Square.square)
      =
    let file_diff =
      Square.file_to_index square1.file - Square.file_to_index square2.file
    in
    (get_piece square1.occupant).name = King && (file_diff > 1 || file_diff < -1)

  let is_king (square2 : Square.square) =
    let occ = square2.occupant in
    match occ with Some x when x.name = King -> true | _ -> false

  let move (board : b) (square1 : Square.square) (square2 : Square.square)
      (state : State.state) (is_engine : bool) =
    if is_king square2 then
      print_endline
        ("TAKING A KING! on:" ^ Char.escaped square2.file
       ^ string_of_int square2.rank);
    if is_valid_move square1 square2 state.turn then (
      let is_ep = is_en_passant square1 square2 in
      let is_promote = is_promotion square1 in
      if is_castle board square1 square2 then (
        let rook_file = if square2.file = 'g' then 7 else 0 in
        let rook_rank = 8 - square1.rank in
        let empty_square_file = if square2.file = 'g' then 5 else 3 in
        let empty_square_rank = rook_rank in
        updatepieceList board
          board.tiles.(rook_rank).(rook_file)
          board.tiles.(empty_square_rank).(empty_square_file)
          is_ep;
        update_tiles board
          board.tiles.(rook_rank).(rook_file)
          board.tiles.(empty_square_rank).(empty_square_file)
          is_ep)
      else ();
      updatepieceList board square1 square2 is_ep;
      update_tiles board square1 square2 is_ep;
      if is_ep then
        if state.turn = White then
          state.white_has_captured.(0) <- state.white_has_captured.(0) + 1
        else state.black_has_captured.(0) <- state.black_has_captured.(0) + 1;
      if is_promote then promote_pawn board state square2 is_engine else ();
      update_next_moves board;
      give_ep board square2 state)
    else raise InvalidMove
end
