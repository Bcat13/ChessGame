open OUnit2
open Board
open Setup
open Piece
open Square
open State

(*  Testing Plan: Due to the highly situational nature of chess as well as the
    mutability we used to implement the project, it is not feasible to test all
    of the game's many edge cases through unit testing. Consequently, we utilize
    unit testing in order to ensure the correctness of the initial state of the
    game. This includes checking that each piece can only move to certain valid
    squares at the start of the game. Meanwhile, extensive manual testing
    (i.e. playing through games by hand) ensures that the correctness of the
    program is maintained despite the dynamic nature of chess. Furthermore, we
    imlement randomized testing by simulating games with random moves, which
    helps us identify issues that would not arise through manual testing. All
    together, these various forms of testing ensure that our project functions
    as intended *)

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether
    they are equivalent set-like lists. That means checking two things.
    First, they must both be {i set-like}, meaning that they do not
    contain any duplicates. Second, they must contain the same elements,
    though not necessarily in the same order. *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  && List.length lst2 = List.length uniq2
  && uniq1 = uniq2

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

let pp_loc (t : Piece.l) =
  "(" ^ Char.escaped (fst t) ^ ", " ^ string_of_int (snd t) ^ ")"

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to
    pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

let pp_array (arr : 'a array) = Array.to_list arr |> pp_list string_of_int

(******************************************************************************)

let (board : Board.b) = { tiles = Setup.board; piece_list = Setup.piecelist }

let get_arr_index_test (name : string) (piece : Piece.piece)
    (expected_output : int array) : test =
  name >:: fun _ ->
  assert_equal expected_output (Board.get_arr_index piece) ~printer:pp_array

let get_arr_index_tests =
  [
    get_arr_index_test "e2 -> [|6;4|] " Setup.pawn5 [| 6; 4 |];
    get_arr_index_test "a1 -> [|7;0|] " Setup.rook1 [| 7; 0 |];
    get_arr_index_test "e1 -> [|7;4|] " Setup.king1 [| 7; 4 |];
    get_arr_index_test "a8 -> [|0;0|] " Setup.rook3 [| 0; 0 |];
    get_arr_index_test "e8 -> [|0;4|] " Setup.king2 [| 0; 4 |];
  ]

let paths_for_pawn_test (name : string) (board : Board.b) (piece : Piece.piece)
    (c_incr : int) (r_incr : int) (is_take : bool)
    (expected_output : Piece.l list) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (Board.paths_for_pawn board piece c_incr r_incr is_take)
    ~cmp:cmp_set_like_lists ~printer:(pp_list pp_loc)

let paths_for_pawn_tests =
  [
    paths_for_pawn_test "initial board: move 1 white" board Setup.pawn5 0 (-1)
      false [ ('e', 3) ];
    paths_for_pawn_test "initial board: move 2 white" board Setup.pawn5 0 (-2)
      false [ ('e', 4) ];
    paths_for_pawn_test "initial board: take left white" board Setup.pawn5 (-1)
      (-1) true [];
    paths_for_pawn_test "initial board: take right white" board Setup.pawn5 1
      (-1) true [];
    paths_for_pawn_test "initial board: move 1 black" board Setup.pawn12 0 1
      false [ ('d', 6) ];
    paths_for_pawn_test "initial board: move 2 white" board Setup.pawn12 0 2
      false [ ('d', 5) ];
    paths_for_pawn_test "initial board: take left black" board Setup.pawn12 1 1
      true [];
    paths_for_pawn_test "initial board: take right black" board Setup.pawn12
      (-1) 1 true [];
  ]

let path_for_piece_test (name : string) (board : Board.b) (piece : Piece.piece)
    (c_incr : int) (r_incr : int) (expected_output : Piece.l list) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (Board.path_for_piece board piece c_incr r_incr)
    ~cmp:cmp_set_like_lists ~printer:(pp_list pp_loc)

let path_for_piece_tests =
  [
    path_for_piece_test "initial board: a1 rook cannot move forward" board
      Setup.rook1 0 (-1) [];
    path_for_piece_test "initial board: a1 rook cannot move backward" board
      Setup.rook1 0 1 [];
    path_for_piece_test "initial board: a1 rook cannot move left" board
      Setup.rook1 (-1) 0 [];
    path_for_piece_test "initial board: a1 rook cannot move right" board
      Setup.rook1 1 0 [];
    path_for_piece_test "initial board: d1 queen cannot move forward" board
      Setup.queen1 0 (-1) [];
    path_for_piece_test "initial board: d1 queen cannot move backward" board
      Setup.queen1 0 1 [];
    path_for_piece_test "initial board: d1 queen cannot move left" board
      Setup.queen1 (-1) 0 [];
    path_for_piece_test "initial board: d1 queen cannot move right" board
      Setup.queen1 1 0 [];
    path_for_piece_test "initial board: d1 queen cannot move forward-left" board
      Setup.queen1 (-1) (-1) [];
    path_for_piece_test "initial board: d1 queen cannot move forward-right"
      board Setup.queen1 1 (-1) [];
    path_for_piece_test "initial board: d1 queen cannot move backward-left"
      board Setup.queen1 (-1) 1 [];
    path_for_piece_test "initial board: d1 queen cannot move backward-right"
      board Setup.queen1 1 1 [];
    path_for_piece_test "initial board: f1 bishop cannot move forward-left"
      board Setup.bishop2 (-1) (-1) [];
    path_for_piece_test "initial board: f1 bishop cannot move forward-right"
      board Setup.bishop2 1 (-1) [];
    path_for_piece_test "initial board: f1 bishop cannot move backward-left"
      board Setup.bishop2 (-1) 1 [];
    path_for_piece_test "initial board: f1 bishop cannot move backward-right"
      board Setup.bishop2 1 1 [];
  ]

let square_for_piece_test (name : string) (board : Board.b)
    (piece : Piece.piece) (c_incr : int) (r_incr : int)
    (expected_output : Piece.l list) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (Board.square_for_piece board piece c_incr r_incr)
    ~cmp:cmp_set_like_lists ~printer:(pp_list pp_loc)

let square_for_piece_tests =
  [
    square_for_piece_test "initial board: b1 knight can move forward then left"
      board Setup.knight1 (-1) (-2) [ ('a', 3) ];
    square_for_piece_test "initial board: b1 knight can move forward then right"
      board Setup.knight1 1 (-2) [ ('c', 3) ];
    square_for_piece_test
      "initial board: b1 knight cannot move right then forward" board
      Setup.knight1 2 (-1) [];
    square_for_piece_test
      "initial board: b1 knight cannot move left then forward" board
      Setup.knight1 (-2) 1 [];
    square_for_piece_test
      "initial board: b1 knight cannot move backward then right" board
      Setup.knight1 1 2 [];
    square_for_piece_test "initial board: white king cannot move forward" board
      Setup.king1 0 (-1) [];
    square_for_piece_test "initial board: white king cannot move backward" board
      Setup.king1 0 1 [];
    square_for_piece_test "initial board: white king cannot move left" board
      Setup.king1 (-1) 0 [];
    square_for_piece_test "initial board: white king cannot move right" board
      Setup.king1 1 0 [];
    square_for_piece_test "initial board: white king cannot move forward-left"
      board Setup.king1 (-1) (-1) [];
    square_for_piece_test "initial board: white king cannot move forward-right"
      board Setup.king1 1 (-1) [];
    square_for_piece_test "initial board: white king cannot move backward-left"
      board Setup.king1 (-1) 1 [];
    square_for_piece_test "initial board: white king cannot move backward-right"
      board Setup.king1 1 1 [];
  ]

let get_piece_test (name : string) (board : Board.b) (square_loc : string)
    (expected_output : Piece.k) : test =
  name >:: fun _ ->
  ((Board.get_square board square_loc).occupant |> Board.get_piece).name
  |> assert_equal expected_output

let move_test (name : string) (board : Board.b) (square1_loc : string)
    (square2_loc : string) (state : State.state) (expected_output : Piece.k) :
    test =
  name >:: fun _ ->
  let square1 = Board.get_square board square1_loc in
  let square2 = Board.get_square board square2_loc in
  Board.move board square1 square2 state true;
  ((Board.get_square board square2_loc).occupant |> Board.get_piece).name
  |> assert_equal expected_output

let get_pieceTests =
  let (board : Board.b) =
    { tiles = Setup.board; piece_list = Setup.piecelist }
  in
  [
    get_piece_test "initial white queen" board "d1" Queen;
    get_piece_test "initial black rook" board "a8" Rook;
  ]

let moveTests =
  let (board : Board.b) =
    { tiles = Setup.board; piece_list = Setup.piecelist }
  in
  [
    move_test "move pawn" board "a2" "a3" Setup.state Pawn;
    move_test "move pawn again" board "a3" "a4" Setup.state Pawn;
    move_test "move knight" board "b1" "c3" Setup.state Knight;
    move_test "move knight back" board "c3" "b1" Setup.state Knight;
  ]

let in_check_test (name : string) (board : Board.b) (turn : Piece.c)
    (expected_output : bool) =
  name >:: fun _ -> assert_equal expected_output (Board.in_check board turn)

let inCheckTests =
  let (board : Board.b) =
    { tiles = Setup.board; piece_list = Setup.piecelist }
  in
  [
    in_check_test "white not initially in check" board White false;
    in_check_test "black not initially in check" board Black false;
  ]

let in_stalemate_test (name : string) (board : Board.b) (turn : Piece.c)
    (expected_output : bool) =
  name >:: fun _ -> assert_equal expected_output (Board.inStalemate board turn)

let inStalemateTests =
  let (board : Board.b) =
    { tiles = Setup.board; piece_list = Setup.piecelist }
  in
  [
    in_stalemate_test "white not initially in stalemate" board White false;
    in_stalemate_test "black not initially in stalemate" board Black false;
  ]

let suite =
  "test suite for Final Project"
  >::: List.flatten
         [
           get_arr_index_tests;
           paths_for_pawn_tests;
           path_for_piece_tests;
           square_for_piece_tests;
           get_pieceTests;
           inCheckTests;
           inStalemateTests;
           moveTests;
         ]

let _ = run_test_tt_main suite
