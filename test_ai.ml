open OUnit2
open Ai
open State
open Command

let lower_left = {cell_coord = (1, 0); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1}
let lower_right = {cell_coord = (1, 1); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 2}
let upper_left = {cell_coord = (0, 0); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 3}
let upper_right = {cell_coord = (0, 1); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1}

let board = [[upper_left;upper_right];[lower_left;lower_right]]

let hard_ai = {name = "hard_ai";
               score = 0;
               rack = [('o', 1);('c', 3);('d', 2);('n', 1);('e', 1);('n', 1);('n', 1)];
               player_type = AI Hard;
               order_num = 1}


let hard_ai_pass = {name = "hard_ai";
                score = 0;
                rack = [];
                player_type = AI Hard;
                order_num = 1}

let hard_ai_swap = {name = "hard_ai";
                 score = 0;
                 rack = [('a',1)];
                 player_type = AI Hard;
                 order_num = 1}

let best_move1 = {word = ['c';'o';'n';'n';'e';'d'];mv_coord = (7,7); is_horizontal = true}
let best_move2 = {word = ['e';'m';'b';'a';'r';'s'];mv_coord = (7,11); is_horizontal = false}
let best_move3 = {word = ['p';'a'];mv_coord = (10,10); is_horizontal = false}

let hard_state = {board = init_board 15;
                  bag = [('s',1);('c',3);('r',1);('a',1);('s',1);('c',3);('r',1);('a',1);
                          ('t',1);('b',3);('m',3);('p',3)];
                        players = [hard_ai];
                        added_words = [];
                        current_player = hard_ai;
                        is_first_move = true;
                  sp_consec = 0}

let hard_state' = do' (PlaceWord best_move1) hard_state
let hard_state'' = do' (PlaceWord best_move2) hard_state'

let hard_state_pass = {board = init_board 15;
                   bag = [];
                   players = [hard_ai_pass];
                   added_words = [];
                   current_player = hard_ai_pass;
                   is_first_move = true;
                   sp_consec = 0}

let hard_state_swap = {board = init_board 15;
                    bag = [('s',1); ('c',3); ('r',1); ('a',1)];
                    players = [hard_ai_swap];
                    added_words = [];
                    current_player = hard_ai_swap;
                    is_first_move = true;
                    sp_consec = 0}

let hint1 = {word = ['p';'r';'i';'z';'e'];mv_coord = (7,7); is_horizontal = true}
let hint2 = {word = ['o';'e'];mv_coord = (6,7); is_horizontal = true}
let hint3 = {word = ['n';'o'];mv_coord = (6,11); is_horizontal = true}


let easy_ai = {name = "easy_ai";
               score = 0;
               rack = [('i', 1);('r', 1);('u', 1);('e', 1);('i', 3);('*', 0);('p', 3)];
               player_type = AI Easy;
               order_num = 1}

let easy_state = {board = init_board 15;
                  bag = [('o', 1);('o', 1);('n', 1);('e', 1)];
                  players = [easy_ai];
                  added_words = [];
                  current_player = easy_ai;
                  is_first_move = true;
                  sp_consec = 0}

let easy_state' = do' (PlaceWord hint1) easy_state
let easy_state'' = do' (PlaceWord hint2) easy_state'

let easy_ai_pass = {name = "easy_ai_pass";
               score = 0;
               rack = [];
               player_type = AI Easy;
               order_num = 1}

let easy_state_pass = {board = init_board 15;
                   bag = [];
                   players = [easy_ai_pass];
                   added_words = [];
                   current_player = easy_ai_pass;
                   is_first_move = true;
                   sp_consec = 0}

let easy_ai_swap = {name = "easy_ai_swap";
               score = 0;
               rack = [('z',10)];
               player_type = AI Easy;
               order_num = 1}

let easy_state_swap = {board = init_board 15;
                        bag = [('x',8)];
                        players = [easy_ai_swap];
                        added_words = [];
                        current_player = easy_ai_swap;
                        is_first_move = true;
                  sp_consec = 0}


let tests = [
  "reverse_empty" >:: (fun _ -> assert_equal "" (reverse_str ""));
  "reverse_str" >:: (fun _ -> assert_equal "abcd" (reverse_str "dcba"));

  "up_cell_some1" >:: (fun _ -> assert_equal (Some (0,0)) (up_cell lower_left));
  "up_cell_some2" >:: (fun _ -> assert_equal (Some (0,1)) (up_cell lower_right));
  "up_cell_none1" >:: (fun _ -> assert_equal None (up_cell upper_left));
  "up_cell_none2" >:: (fun _ -> assert_equal None (up_cell upper_right));

  "left_cell_some1" >:: (fun _ -> assert_equal (Some (0,0)) (left_cell upper_right));
  "left_cell_some2" >:: (fun _ -> assert_equal (Some (1,0)) (left_cell lower_right));
  "left_cell_none1" >:: (fun _ -> assert_equal None (left_cell upper_left));
  "left_cell_none2" >:: (fun _ -> assert_equal None (left_cell lower_left));

  "down_cell_some1" >:: (fun _ -> assert_equal (Some (1,0)) (down_cell upper_left));
  "down_cell_some2" >:: (fun _ -> assert_equal (Some (1,1)) (down_cell upper_right));

  "right_cell_some1" >:: (fun _ -> assert_equal (Some (0,1)) (right_cell upper_left));
  "right_cell_some2" >:: (fun _ -> assert_equal (Some (1,1)) (right_cell lower_left));

  "best_move1" >:: (fun _ -> assert_equal (PlaceWord best_move1) (best_move hard_state));
  "best_move2" >:: (fun _ -> assert_equal (PlaceWord best_move2) (best_move hard_state'));
  "best_move3" >:: (fun _ -> assert_equal (PlaceWord best_move3) (best_move hard_state''));
  "best_move_pass" >:: (fun _ -> assert_equal (Pass) (best_move hard_state_pass));
  "best_move_swap" >:: (fun _ -> assert_equal (Swap ['a']) (best_move hard_state_swap));


  "hint1" >:: (fun _ -> assert_equal (PlaceWord hint1) (get_hint easy_state));
  "hint2" >:: (fun _ -> assert_equal (PlaceWord hint2) (get_hint easy_state'));
  "hint3" >:: (fun _ -> assert_equal (PlaceWord hint3) (get_hint easy_state''));
  "hint_pass" >:: (fun _ -> assert_equal (Pass) (get_hint easy_state_pass));
  "hint_swap" >:: (fun _ -> assert_equal (Swap ['z']) (get_hint easy_state_swap));
]
