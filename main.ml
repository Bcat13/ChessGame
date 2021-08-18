open Board
open State
open Square
open Piece
open Setup
open Command
open Engine
open OUnit2

(** [pieces_taken] prints a Unicode piece that have been taken by white and 
black*)
let pieces_taken color (state : State.state) =
  let taken_array =
    if color = Black then state.white_has_captured else state.black_has_captured
  in
  for i = 0 to 4 do
    for x = 1 to taken_array.(i) do
      if i = 0 then print_string (if color = White then "♙" else "♟")
      else if i = 1 then print_string (if color = White then "♘" else "♞")
      else if i = 2 then print_string (if color = White then "♗" else "♝")
      else if i = 3 then print_string (if color = White then "♖" else "♜")
      else print_string (if color = White then "♕" else "♛");
      if x = taken_array.(i) then print_string " " else print_string ""
    done
  done

(** [pp_piece] prints an Unicode chess piece in a board*)
let pp_piece (piece : Piece.piece option) =
  match piece with
  | None -> print_string "|   "
  | Some x -> (
      if x.color = White then
        match x.name with
        | Pawn -> print_string "| ♙ "
        | Rook -> print_string "| ♖ "
        | Knight -> print_string "| ♘ "
        | Bishop -> print_string "| ♗ "
        | Queen -> print_string "| ♕ "
        | King -> print_string "| ♔ "
      else
        match x.name with
        | Pawn -> print_string "| ♟ "
        | Rook -> print_string "| ♜ "
        | Knight -> print_string "| ♞ "
        | Bishop -> print_string "| ♝ "
        | Queen -> print_string "| ♛ "
        | King -> print_string "| ♚ ")

(** [pp_row] prints a row for a board given a [board] and [state] *)
let pp_row (row : Square.square array) : unit =
  for i = 0 to Array.length row - 1 do
    pp_piece row.(i).occupant
  done

(** [pp_row_flipped] prints a row for a board rotated 180 degrees given a 
[board] and [state] *)
let pp_row_flipped (row : Square.square array) : unit =
  for i = 0 to Array.length row - 1 do
    pp_piece row.(7 - i).occupant
  done

(** [pp_board_flipped] prints a board rotated 180 degrees given a [board] and 
[state] *)
let pp_board_flipped (board : Board.b) state =
  print_endline
    "   ――― ――― ――― ――― ――― ――― ――― \
     ―――";
  for i = 0 to 7 do
    string_of_int (i + 1) ^ " " |> print_string;
    pp_row_flipped board.tiles.(7 - i);
    if i = 7 then (
      print_string "| ";
      pieces_taken White state;
      if Board.get_game_score board >= 0 then ()
      else
        print_string
          (" +" ^ (Board.get_game_score board |> Int.abs |> string_of_int));
      print_endline "")
    else if i = 0 then (
      print_string "| ";
      pieces_taken Black state;

      if Board.get_game_score board <= 0 then ()
      else
        print_string
          (" +" ^ (Board.get_game_score board |> Int.abs |> string_of_int));
      print_endline "")
    else print_endline "| ";
    print_endline
      "   ――― ――― ――― ――― ――― ――― \
       ――― ―――"
  done;
  print_endline "    H   G   F   E   D   C   B   A  "

(** [pp_board] prints the board given the [board] and [state] *)
let pp_board (board : Board.b) state =
  print_endline
    "   ――― ――― ――― ――― ――― ――― ――― \
     ―――";
  for i = 0 to 7 do
    string_of_int (8 - i) ^ " " |> print_string;
    pp_row board.tiles.(i);
    if i = 0 then (
      print_string "| ";
      pieces_taken White state;
      if Board.get_game_score board >= 0 then ()
      else
        print_string
          (" +" ^ (Board.get_game_score board |> Int.abs |> string_of_int));
      print_endline "")
    else if i = 7 then (
      print_string "| ";
      pieces_taken Black state;

      if Board.get_game_score board <= 0 then ()
      else
        print_string
          (" +" ^ (Board.get_game_score board |> Int.abs |> string_of_int));
      print_endline "")
    else print_endline "| ";
    print_endline
      "   ――― ――― ――― ――― ――― ――― \
       ――― ―――"
  done;
  print_endline "    A   B   C   D   E   F   G   H  "

let game_over_test (state : State.state) =
  state.checkmate_black || state.checkmate_white || state.stalemate_black
  || state.stalemate_white

let engine_move_test (board : Board.b) (state : State.state) game_type i =
  try
    let pl =
      List.filter
        (fun (x : Piece.piece) -> x.color = state.turn)
        board.piece_list
    in
    let moves = Array.of_list (Engine.get_next_moves board pl []) in
    Random.self_init ();
    let rand = Random.int (Array.length moves) in
    let move = moves.(rand) in
    let square1 = fst move in
    let square2 = snd move in
    let square2_occ = square2.occupant in
    Board.move board square1 square2 state true;
    let temp_st = state |> Board.update_state board square2_occ in
    if game_over_test temp_st then print_endline ("Success " ^ string_of_int i)
    else game_type false board temp_st i
  with _ ->
    print_endline "ERROR";
    exit 0

(** [game_over_printer] prints if black or white is in a finishing game state
   given the current [state] and subsequently prints the [board]*)
let game_over_printer (state : State.state) (board : Board.b) =
  if state.checkmate_black then (
    if state.turn = White then pp_board board state
    else pp_board_flipped board state;
    print_endline "Checkmate. White wins!\n";
    exit 0)
  else if state.checkmate_white then (
    if state.turn = White then pp_board board state
    else pp_board_flipped board state;
    print_endline "Checkmate. Black wins!\n";
    exit 0)
  else if state.stalemate_black || state.stalemate_white then (
    if state.turn = White then pp_board board state
    else pp_board_flipped board state;
    print_endline "Stalemate. It's a draw!\n";
    exit 0)
  else print_string "";
  ()

(** [in_check_printer] prints if black or white is in check in a given [state]*)
let in_check_printer (state : State.state) =
  if state.in_check_white && state.turn = White then
    print_endline "White is in check!"
  else if state.in_check_black && state.turn = Black then
    print_endline "Black is in check!"
  else print_string "";
  ()

(** [engine_move_random] makes a random move on a specific [board] with a given 
[state]*)
let engine_move_random (board : Board.b) (state : State.state) game_type =
  game_over_printer state board;
  in_check_printer state;
  if state.turn = White then pp_board board state
  else pp_board_flipped board state;
  print_endline "";
  let pl =
    List.filter (fun (x : Piece.piece) -> x.color = state.turn) board.piece_list
  in
  let moves = Array.of_list (Engine.get_next_moves board pl []) in
  Random.self_init ();
  let rand = Random.int (Array.length moves) in
  let move = moves.(rand) in
  let square1 = fst move in
  let square2 = snd move in
  let square2_occ = square2.occupant in
  Board.move board square1 square2 state true;
  state |> Board.update_state board square2_occ |> game_type false board

(** [engine_move_minimax] makes a move using the minimax algorith on a specific
 [board] with a given [state]*)
let engine_move_minimax depth (board : Board.b) (state : State.state) game_type
    =
  game_over_printer state board;
  in_check_printer state;
  let move =
    snd (Engine.minimax board state depth (Square.empty, Square.empty) true)
  in
  let square1 = fst move in
  let square2 = snd move in
  let square2_occ = square2.occupant in
  Board.move board square1 square2 state true;
  state |> Board.update_state board square2_occ |> game_type depth false board

(** [player_move] prompts a player to make a move on a specific [board] 
  with a given [state] and makes that move if it is legal move in chess*)
let player_move depth (board : Board.b) (state : State.state) game_type
    (prev_malformed : bool) =
  game_over_printer state board;
  if prev_malformed then ()
  else if state.turn = White then pp_board board state
  else pp_board_flipped board state;
  in_check_printer state;
  let person_move = State.string_of_turn state.turn in
  print_endline (person_move ^ "'s turn");
  print_string "> ";
  match read_line () with
  | move -> (
      try
        match Command.parse_move move with
        | Quit ->
            print_endline (person_move ^ " resigns\n");
            exit 0
        | Move x ->
            let square1 = Board.get_square board x.(0) in
            let square2 = Board.get_square board x.(1) in
            let square2_occ = square2.occupant in
            Board.move board square1 square2 state false;
            state
            |> Board.update_state board square2_occ
            |> game_type depth false board
      with
      | Command.Malformed ->
          print_endline
            {|Error! Command needs to be of the form "<letter><number> to <letter><number>"|};
          game_type depth true board state
      | Board.InvalidSquare ->
          print_endline
            {|Error! <letter> must be in a..h and <number> must be in 1..8|};
          game_type depth true board state
      | Board.InvalidMove ->
          print_endline {|Error! Piece cannot move to this square.|};
          game_type depth true board state
      | Board.EmptySquare ->
          print_endline
            {|Error! There is no piece to move on the specified square.|};
          game_type depth true board state
      | _ -> print_endline "unknown error")

(** [two_player_game] is the game mode where two humans play against each 
other*)
let rec two_player_game depth (prev_malformed : bool) (board : Board.b)
    (state : State.state) =
  player_move depth
    (board : Board.b)
    (state : State.state)
    two_player_game prev_malformed

(** [one_player_game] is the game mode where the human plays as white and 
the engine plays as black. *)
let rec one_player_game depth (prev_malformed : bool) (board : Board.b)
    (state : State.state) =
  if state.turn = Black then
    engine_move_minimax depth
      (board : Board.b)
      (state : State.state)
      one_player_game
  else
    player_move depth
      (board : Board.b)
      (state : State.state)
      one_player_game prev_malformed

(** [one_player_game_black] is the game mode where the human plays as black and 
the engine plays as white. *)
let rec one_player_game_black depth (prev_malformed : bool) (board : Board.b)
    (state : State.state) =
  if state.turn = White then
    engine_move_minimax depth
      (board : Board.b)
      (state : State.state)
      one_player_game_black
  else
    player_move depth
      (board : Board.b)
      (state : State.state)
      one_player_game_black prev_malformed

(** [engine_vs_engine] is the game mode where two engines play against 
each other *)
let rec engine_vs_engine (prev_malformed : bool) (board : Board.b)
    (state : State.state) =
  engine_move_random (board : Board.b) (state : State.state) engine_vs_engine

let rec engine_vs_engine_testing (prev_malformed : bool) (board : Board.b)
    (state : State.state) i =
  engine_move_test
    (board : Board.b)
    (state : State.state)
    engine_vs_engine_testing i

(** [initialize_game ()] prompts the player to select a game type and starts 
it*)
let rec initialize_game () =
  let (board : Board.b) =
    { tiles = Setup.board; piece_list = Setup.piecelist }
  in
  let (state : State.state) = Setup.state in
  print_endline "Type \"1\" for One Player mode";
  print_endline "Type \"2\" for Two Player mode";
  print_endline "Type \"spectate\" to watch two engines play";
  print_endline "Type \"quit\" to leave the game";
  print_string "> ";
  match read_line () with
  | cmd -> (
      try
        match Command.parse_player_command cmd with
        | Q ->
            print_endline "Ending game\n";
            exit 0
        | Two -> two_player_game 0 false board state
        | One -> (
            print_endline "\nPick a strength for the Engine";
            print_string "> ";
            match read_line () with
            | x -> (
                match Command.parse_engine_depth x with
                | depth -> (
                    print_endline "\n\"White\" or \"Black\"";
                    print_string "> ";
                    match read_line () with
                    | color -> (
                        match Command.parse_player_color color with
                        | Black -> one_player_game_black depth false board state
                        | White -> one_player_game depth false board state))))
        | Sim -> engine_vs_engine false board state
        | Sim2 x ->
            for i = 1 to x do
              let bc = Board.copy_board board in
              let st_copy = State.state_copy state in
              engine_vs_engine_testing false bc st_copy i
            done;
            print_endline "All Games Ran Successfully";
            exit 0
      with
      | Command.Malformed -> initialize_game ()
      | e -> raise e)

(** [main ()] Welcomes the player to the game and initializes the game*)
let main () =
  print_endline "\nWelcome to Chess! \n";
  initialize_game ()

(* Execute the game engine. *)
let () = main ()
