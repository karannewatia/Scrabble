open OUnit2
open State
open Command

let board_1 = [[{cell_coord = (0, 0); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 3}]]

let board_2 = [[{cell_coord = (0, 0); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 3};
                {cell_coord = (0, 1); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1}];
               [{cell_coord = (1, 0); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 1};
                {cell_coord = (1, 1); letter = (' ', -1); letter_multiplier = 1; word_multiplier = 2}]]

let player1 = {name = "arman";
               score = 0;
               rack = [('s', 1);('p', 3);('s', 1);('o', 1);('p', 3);('p', 3);('u', 1)];
               player_type = Human;
               order_num = 1}

let player2 = {name = "connor";
               score = 0;
               rack = [('t', 1);('m', 3);('m', 3);('i', 1);('m', 3);('m', 3);('r', 1)];
               player_type = Human;
               order_num = 2}

let player_wildcard = {name = "wildcard";
                       score = 0;
                       rack = [('s', 1);('p', 3);('s', 1);('*', 0);('p', 3);('p', 3);('u', 1)];
                       player_type = Human;
                       order_num = 1}

let init_game_data_1h = {
  num_players = 1;
  num_humans = 1;
  ai_difficulty = [];
  human_names = ["foo"];
}
let init_game_data_1ai = {
  num_players = 1;
  num_humans = 0;
  ai_difficulty = [Easy];
  human_names = [];
}
let init_game_data_2h = {
  num_players = 2;
  num_humans = 2;
  ai_difficulty = [];
  human_names = ["foo";"bar"];
}
let init_game_data_2ai = {
  num_players = 2;
  num_humans = 0;
  ai_difficulty = [Easy;Hard];
  human_names = [];
}
let init_game_data_4h = {
  num_players = 4;
  num_humans = 4;
  ai_difficulty = [];
  human_names = ["foo";"bar";"hello";"world"];
}
let init_game_data_1h3ai = {
  num_players = 4;
  num_humans = 1;
  ai_difficulty = [Easy;Easy;Hard];
  human_names = ["foo"];
}

let basic_state_1bag = {board = init_board 15;
                        bag = [('z', 10)];
                        players = [player1; player2];
                        added_words = [];
                        current_player = player1;
                        is_first_move = true;
                        sp_consec = 0}

let basic_state_2bag = {board = init_board 15;
                        bag = [('z', 10); ('k', 5)];
                        players = [player1; player2];
                        added_words = [];
                        current_player = player1;
                        is_first_move = true;
                        sp_consec = 0}

let wildcard_player_state = {board = init_board 15;
                             bag = [('z', 10); ('k', 5);('a', 1)];
                             players = [player_wildcard; player2];
                             added_words = [];
                             current_player = player_wildcard;
                             is_first_move = true;
                             sp_consec = 0}

let move1_h = {word = ['s';'o'];
               mv_coord = (7,7);
               is_horizontal = true}

let move1_v = {word = ['s';'o'];
               mv_coord = (7,7);
               is_horizontal = false}

let move2_h = {word = ['s';'o';'r';'t'];
               mv_coord = (7,7);
               is_horizontal = true}

let move2_v = {word = ['s';'o';'r';'t'];
               mv_coord = (7,7);
               is_horizontal = false}

(* [get_prev_player n p] returns the player whose turn was before the
 * current player with order number [n] given a list of the players [p]. *)
let rec get_prev_player n p =
  let n' =
    if n = 1 then List.length p
    else n - 1
  in
  List.hd (List.filter (fun p' -> p'.order_num = n') p)

(* [check_racks players] checks that all [players] have 7 letters in rack *)
let rec check_racks players =
  List.for_all (fun p -> List.length p.rack = 7) players

(* [check_order_nums players] checks that all [players] have unique order
 * numbers. *)
let rec check_order_nums players =
  let order_nums = List.fold_left (fun acc p -> p.order_num :: acc) [] players in
  List.length (List.sort Pervasives.compare order_nums) = List.length order_nums

(* [verify_board c st is_h word] returns true if the word placed at coordinate
 * [c] in the direction specified by [is_h] is equal to [word]. *)
let verify_board c st is_h word =
  match (get_adjacent_word c st is_h []) with
  | Some (s, _,_) -> s = word
  | None -> false

let init_state_tests = [
  (* init_board tests. *)
  "init_board_1" >:: (fun _ -> assert_equal board_1 (init_board 1));
  "init_board_2" >:: (fun _ -> assert_equal board_2 (init_board 2));

  (* bag tests *)
  "init_bag_1h" >:: (fun _ ->
      assert_equal 93 (List.length (init_state init_game_data_1h).bag));
  "init_bag_1ai" >:: (fun _ ->
      assert_equal 93 (List.length (init_state init_game_data_1ai).bag));
  "init_bag_2h" >:: (fun _ ->
      assert_equal 86 (List.length (init_state init_game_data_2h).bag));
  "init_bag_2ai" >:: (fun _ ->
      assert_equal 86 (List.length (init_state init_game_data_2ai).bag));
  "init_bag_4h" >:: (fun _ ->
      assert_equal 72 (List.length (init_state init_game_data_4h).bag));
  "init_bag_1h3ai" >:: (fun _ ->
      assert_equal 72 (List.length (init_state init_game_data_1h3ai).bag));

  (* player rack tests *)
  "init_rack_1h" >:: (fun _ ->
      assert_equal 7 (List.length (init_state init_game_data_1h).current_player.rack));
  "init_rack_1ai" >:: (fun _ ->
      assert_equal 7 (List.length (init_state init_game_data_1ai).current_player.rack));
  "init_rack_2h" >:: (fun _ ->
      assert_equal true (check_racks (init_state init_game_data_1ai).players));
  "init_rack_2ai" >:: (fun _ ->
      assert_equal true (check_racks (init_state init_game_data_2ai).players));
  "init_rack_4h" >:: (fun _ ->
      assert_equal true (check_racks (init_state init_game_data_4h).players));
  "init_rack_1h3ai" >:: (fun _ ->
      assert_equal true (check_racks (init_state init_game_data_1h3ai).players));

  (* player order_num tests *)
  "init_order_num_1h" >:: (fun _ ->
      assert_equal true (check_order_nums (init_state init_game_data_1h).players));
  "init_order_num_1ai" >:: (fun _ ->
      assert_equal true (check_order_nums (init_state init_game_data_1ai).players));
  "init_order_num_2h" >:: (fun _ ->
      assert_equal true (check_order_nums (init_state init_game_data_2h).players));
  "init_order_num_2ai" >:: (fun _ ->
      assert_equal true (check_order_nums (init_state init_game_data_2ai).players));
  "init_order_num_4h" >:: (fun _ ->
      assert_equal true (check_order_nums (init_state init_game_data_4h).players));
  "init_order_num_1h3ai" >:: (fun _ ->
      assert_equal true (check_order_nums (init_state init_game_data_1h3ai).players));

  (* sp_consec test *)
  "init_sp_consec" >:: (fun _ -> assert_equal 0 (init_state init_game_data_4h).sp_consec);
]

let add_word_tests = [
  (* add_word tests. *)
  "add_word_basic" >:: (fun _ ->
      assert_equal ["blah"] (do' (AddWord "blah") basic_state_1bag).added_words);
  "add_word_invalid" >:: (fun _ ->
      let e = fun () -> (do' (AddWord "3110") basic_state_1bag).added_words in
      assert_raises InvalidAdd e);
  "add_word_invalid1" >:: (fun _ ->
      let e = fun () -> (do' (AddWord "CS3110") basic_state_1bag).added_words in
      assert_raises InvalidAdd e);
  "add_word_invalid2" >:: (fun _ ->
      let e = fun () -> (do' (AddWord "CS A") basic_state_1bag).added_words in
      assert_raises InvalidAdd e);
]

let swap_tests = [
  (* swap tests. *)
  "swap1_basic_rack" >:: (fun _ ->
      assert_equal [('z', 10);('p', 3);('s', 1);('o', 1);('p', 3);('p', 3);('u', 1)]
        (let st = (do' (Swap ['s']) basic_state_1bag) in (get_prev_player 2 st.players).rack));
  "swap1_basic_bag" >:: (fun _ ->
      assert_equal [('s', 1)]
        (do' (Swap ['s']) basic_state_1bag).bag);
  "swap2_basic_rack" >:: (fun _ ->
      let st = (do' (Swap ['s'; 'u']) basic_state_2bag) in
      let rack' = (get_prev_player 2 st.players).rack in
      assert_equal true ((List.mem_assoc 'z' rack') && (List.mem_assoc 'k' rack')
                        && List.mem_assoc 's' rack'));
  "swap2_basic_bag" >:: (fun _ ->
      let bag' = (do' (Swap ['s'; 'u']) basic_state_2bag).bag in
      assert_equal true ((List.mem_assoc 's' bag') && (List.mem_assoc 'u' bag')));
  "swap1_exn_bag_too_small" >:: (fun _ ->
      let e = fun () -> do' (Swap ['s';'o']) basic_state_1bag in
      assert_raises InvalidSwap e);
  "swap1_exn_not_in_rack" >:: (fun _ ->
      let e = fun () -> do' (Swap ['l']) basic_state_1bag in
      assert_raises InvalidSwap e);
  "swap_sp_consec" >:: (fun _ ->
      let st = (do' (Swap ['s'; 'u']) basic_state_2bag) in
      assert_equal 1 st.sp_consec);
]

let blank_player = {name = "foo";
                    score = 0;
                    rack = [('*', 0); ('f',4); ('n',1)];
                    player_type = Human;
                    order_num = 1}
let blank_player2 = {blank_player with rack = [('*',0); ('*',0); ('n',1)]}
let camel_player = {blank_player with rack = [('e',1); ('c',3); ('a',1); ('m',3); ('l',1); ('s',1)]}
let it_no_player = {blank_player with rack = [('i',1); ('t',1); ('n',1); ('o',1)]}
let scrabble_player = {blank_player with rack =
  [('s',1); ('c',3); ('r',1); ('a',1); ('b',3); ('b',3); ('l',1); ('e',1); ('i',1); ('f',4); ('i',1); ('t',1)]}

let blank1_state = {board = init_board 15;
                    bag = [('f',4);('f',4);('f',4);('f',4);('f',4);('f',4);
                           ('f',4);('f',4);('f',4);('f',4);('f',4);('f',4);('f',4);('f',4);];
                    players = [blank_player];
                    added_words = [];
                    current_player = blank_player;
                    is_first_move = true;
                    sp_consec = 0}
let blank2_state = {blank1_state with players = [blank_player2];current_player = blank_player2;}
let basic_state = {blank1_state with players = [camel_player];current_player = camel_player;}
let it_no_state = {blank1_state with players = [it_no_player];current_player = it_no_player;}
let scrabble_state = {blank1_state with players = [scrabble_player];current_player = scrabble_player}

let blank_mv1 = {word=['f';'u';'n'];mv_coord=(7,7);is_horizontal=true}
let blank_mv2 = {word=['f';'i';'n'];mv_coord=(7,7);is_horizontal=false}
let blank_mv3 = {word=['n';'o';'t'];mv_coord=(7,7);is_horizontal=true}
let blank_mv4 = {word=['p';'i';'n'];mv_coord=(7,7);is_horizontal=false}

let basic_mv1 = {word=['c';'a';'m';'e';'l'];mv_coord=(7,7);is_horizontal=true}
let basic_mv2 = {word=['c';'a';'m';'e';'l'];mv_coord=(6,8);is_horizontal=false}
let basic_mv3 = {word=['c';'a';'m';'e';'l'];mv_coord=(8,6);is_horizontal=true}
let basic_mv4 = {word=['c';'a';'m';'e';'l';'s'];mv_coord=(7,7);is_horizontal=true}
let basic_mv5 = {word=['c';'a';'m';'e';'l';'s'];mv_coord=(7,7);is_horizontal=false}
let basic_mv6 = {word=['c';'a';'m';'e';'l'];mv_coord=(4,10);is_horizontal=false}

let it_mv1_h = {word=['i';'t'];mv_coord=(7,7);is_horizontal=true}
let no_mv1_h = {word=['n';'o'];mv_coord=(7,7);is_horizontal=true}
let no_mv1_h_below = {word=['n';'o'];mv_coord=(8,7);is_horizontal=true}
let it_mv1_h_above = {word=['i';'t'];mv_coord=(6,7);is_horizontal=true}
let it_mv2_v = {word=['i';'t'];mv_coord=(7,7);is_horizontal=false}
let no_mv2_v = {word=['n';'o'];mv_coord=(7,7);is_horizontal=false}
let no_mv2_v_right = {word=['n';'o'];mv_coord=(7,8);is_horizontal=false}
let it_mv2_v_left = {word=['i';'t'];mv_coord=(7,6);is_horizontal=false}

let scrabble_setup_vert1 = {word=['i';'f'];mv_coord=(7,7);is_horizontal=false}
let scrabble_setup_vert2 = {word=['f';'i';'s';'t'];mv_coord=(8,7);is_horizontal=true}
let scrabble_vert_outofbounds =
  {word=['s';'c';'r';'a';'b';'b';'l';'e'];mv_coord=(8,9);is_horizontal=false}
let scrabble_setup_hor1 = {word=['i';'f'];mv_coord=(7,7);is_horizontal=true}
let scrabble_setup_hor2 = {word=['f';'i';'s';'t'];mv_coord=(7,8);is_horizontal=false}
let scrabble_hor_outofbounds =
  {word=['s';'c';'r';'a';'b';'b';'l';'e'];mv_coord=(9,9);is_horizontal=true}

let disconnected_mv = {word=['a';'b';'l';'e'];mv_coord=(1,1);is_horizontal=true}

let camel_hor_st =
  let st = (do' (PlaceWord basic_mv1) basic_state) in
  {st with current_player = {camel_player with score=10}}
let camel_vert_st =
  let st = (do' (PlaceWord {basic_mv1 with is_horizontal=false}) basic_state) in
  {st with current_player = {camel_player with score=10}}

let it_hor_st =
  let st = (do' (PlaceWord it_mv1_h) it_no_state) in
  {st with current_player = {it_no_player with score=2}}
let it_vert_st =
  let st = (do' (PlaceWord it_mv2_v) it_no_state) in
  {st with current_player = {it_no_player with score=2}}

let scrabble_vert_setup_st =
  let st = (do' (PlaceWord scrabble_setup_vert1) scrabble_state) in
  (do' (PlaceWord scrabble_setup_vert2) st)
let scrabble_hor_setup_st =
  let st = (do' (PlaceWord scrabble_setup_hor1) scrabble_state) in
  (do' (PlaceWord scrabble_setup_hor2) st)

let move1_h_invalid = {word = ['s'; 'u'];
                       mv_coord = (7, 7);
                       is_horizontal = true}

let move1_v_invalid = {word = ['s'; 'u'];
                       mv_coord = (7, 7);
                       is_horizontal = false}

let move_custom_word = {word = ['s'; 'o'; 'p'];
                        mv_coord = (7, 7);
                        is_horizontal = true}

let prefix_player = {name = "David";
                     score = 0;
                     rack = [('t', 1);('a', 1);('c', 3);('h', 4);('m', 3);('m', 3);('r', 1)];
                     player_type = Human;
                     order_num = 1}

let prefix_state = {board = init_board 15;
                    bag = [('n', 1); ('d', 2); ('h', 4); ('v', 4)];
                    players = [prefix_player];
                    added_words = [];
                    current_player = prefix_player;
                    is_first_move= true;
                    sp_consec = 0}

let move1_h_hat = {word = ['h'; 'a'; 't'];
                   mv_coord = (7, 7);
                   is_horizontal = true}

let move1_v_hat = {word = ['h'; 'a'; 't'];
                   mv_coord = (7, 7);
                   is_horizontal = false}

let move2_h_chat = {word = ['c'; 'h'; 'a'; 't'];
                    mv_coord = (7, 6);
                    is_horizontal = true}

let move2_v_chat = {word = ['c'; 'h'; 'a'; 't'];
                    mv_coord = (6, 7);
                    is_horizontal = false}

let place_tests = [
  (* Blank tile tests *)
  "blank_score_1" >:: (fun _ ->
      assert_equal 10 (do' (PlaceWord blank_mv1) blank1_state).current_player.score);
  "blank_score_2" >:: (fun _ ->
      assert_equal 10 (do' (PlaceWord blank_mv2) blank1_state).current_player.score);
  "blank_rack_no_blank" >:: (fun _ ->
      assert_equal false (let r = ((do' (PlaceWord blank_mv2) blank1_state).current_player.rack) in
                          List.mem '*' (List.map (fun (c,_)->c) r)));
  "blank_score_3" >:: (fun _ ->
      assert_equal 2 (do' (PlaceWord blank_mv3) blank2_state).current_player.score);
  "blank_score_4" >:: (fun _ ->
      assert_equal 2 (do' (PlaceWord blank_mv4) blank2_state).current_player.score);
  "blank_rack_no_blanks" >:: (fun _ ->
      assert_equal false (let r = ((do' (PlaceWord blank_mv2) blank2_state).current_player.rack) in
                          List.mem '*' (List.map (fun (c,_)->c) r)));

  (* Score tests *)
  "basic_score_hor" >:: (fun _ ->
      assert_equal 20 (do' (PlaceWord basic_mv1) basic_state).current_player.score);
  "basic_score_vert" >:: (fun _ ->
      assert_equal 20
        (do' (PlaceWord {basic_mv1 with is_horizontal=false}) basic_state).current_player.score);
  "score_vert_intersect_hor" >:: (fun _ ->
      assert_equal 25 (do' (PlaceWord basic_mv2) camel_hor_st).current_player.score);
  "score_hor_intersect_vert" >:: (fun _ ->
      assert_equal 25 (do' (PlaceWord basic_mv3) camel_vert_st).current_player.score);
  "score_hor_hor" >:: (fun _ ->
      assert_equal 10 (do' (PlaceWord no_mv1_h_below) it_hor_st).current_player.score);
  "score_vert_vert" >:: (fun _ ->
      assert_equal 10 (do' (PlaceWord no_mv2_v_right) it_vert_st).current_player.score);
  "score_hor_overlap" >:: (fun _ ->
      assert_equal 20 (do' (PlaceWord basic_mv4) camel_hor_st).current_player.score);
  "score_vert_overlap" >:: (fun _ ->
      assert_equal 20 (do' (PlaceWord basic_mv5) camel_vert_st).current_player.score);
  "score_word_multiplier" >:: (fun _ ->
      assert_equal 28 (do' (PlaceWord basic_mv6) camel_hor_st).current_player.score);

  (* Rack tests. *)
  "place1_rack_horizontal" >:: (fun _ ->
      assert_equal true
        (let st = (do' (PlaceWord move1_h) basic_state_2bag) in
         let lst_chars = ['z';'k';'p';'s';'u'] in
         let prev_rack = (get_prev_player 2 st.players).rack in
         List.for_all (fun c -> List.mem_assoc c prev_rack) lst_chars
         && List.length prev_rack = 7));

  "place1_rack_vertical" >:: (fun _ ->
      assert_equal true
        (let st = (do' (PlaceWord move1_v) basic_state_2bag) in
         let lst_chars = ['z';'k';'p';'s';'u'] in
         let prev_rack = (get_prev_player 2 st.players).rack in
         List.for_all (fun c -> List.mem_assoc c prev_rack) lst_chars
         && List.length prev_rack = 7));

  "place2_rack_horizontal" >:: (fun _ ->
      assert_equal true
        (let st = {(do' (PlaceWord move1_h) basic_state_2bag)
                   with bag = [('q',10);('a',1)]} in
         let st' = (do' (PlaceWord move2_h) st) in
         let lst_chars = ['q';'a';'i';'m'] in
         let prev_rack = (get_prev_player 1 st'.players).rack in
         List.for_all (fun c -> List.mem_assoc c prev_rack) lst_chars
         && List.length prev_rack = 7));

  "place2_rack_vertical" >:: (fun _ ->
    assert_equal true
      (let st = {(do' (PlaceWord move1_v) basic_state_2bag)
                 with bag = [('q',10);('a',1)]} in
       let st' = (do' (PlaceWord move2_v) st) in
       let lst_chars = ['q';'a';'i';'m'] in
       let prev_rack = (get_prev_player 1 st'.players).rack in
       List.for_all (fun c -> List.mem_assoc c prev_rack) lst_chars
       && List.length prev_rack = 7));

  "place2_rack_horizontal" >:: (fun _ ->
      assert_equal 1
        (let st = {(do' (PlaceWord move1_h) basic_state_2bag)
                   with bag = [('q',10);('a',1);('a',1)]} in
         List.length ((do' (PlaceWord move2_h) st).bag)));

  "place2_rack_vertical" >:: (fun _ ->
      assert_equal 1
        (let st = {(do' (PlaceWord move1_v) basic_state_2bag)
                   with bag = [('q',10);('a',1);('a',1)]} in
         List.length ((do' (PlaceWord move2_v) st).bag)));

  (* Bag tests. *)
  "place1_bag_horizontal" >:: (fun _ ->
      assert_equal [] (do' (PlaceWord move1_h) basic_state_2bag).bag);

  "place1_bag_vertical" >:: (fun _ ->
      assert_equal [] (do' (PlaceWord move1_v) basic_state_2bag).bag);

  "place1_bag_wildcard" >:: (fun _ ->
      assert_equal 1 (List.length (do' (PlaceWord move1_h) wildcard_player_state).bag));

  "place2_bag_too_small" >:: (fun _ ->
      assert_equal true
        (let st = (do' (PlaceWord move1_h) basic_state_2bag) in
         let st' = (do' (PlaceWord move2_h) st) in
         let lst_chars = ['i';'m'] in
         let prev_rack = (get_prev_player 1 st'.players).rack in
         List.for_all (fun c -> List.mem_assoc c prev_rack) lst_chars
         && List.length prev_rack = 5));

  (* Next player test. *)
  "place2_next_player_overflow" >:: (fun _ ->
      assert_equal 1
        (let st = {(do' (PlaceWord move1_v) basic_state_2bag)
                   with bag = [('q',10);('a',1)]} in
         let st' = (do' (PlaceWord move2_v) st) in
         st'.current_player.order_num));

  (* Board tests. *)
  "basic_place_horizontal" >:: (fun _ ->
      assert_equal true (let st = (do' (PlaceWord move1_h) basic_state_2bag) in
                         verify_board (7, 7) st true "so"));

  "basic_place_vertical" >:: (fun _ ->
      assert_equal true (let st = (do' (PlaceWord move1_v) basic_state_2bag) in
                         verify_board (7, 7) st false "so"));

  "place2_horizontal" >:: (fun _ ->
      assert_equal true
        (let st = (do' (PlaceWord move1_h) basic_state_2bag) in
         let st' = (do' (PlaceWord move2_h) st) in
         verify_board (7, 7) st' true "sort"));

  "place2_horizontal" >:: (fun _ ->
      assert_equal true
        (let st = (do' (PlaceWord move1_h) basic_state_2bag) in
         let st' = (do' (PlaceWord move2_h) st) in
         verify_board (7, 7) st' true "sort"));

  "place2_vertical" >:: (fun _ ->
      assert_equal true
        (let st = (do' (PlaceWord move1_v) basic_state_2bag) in
         let st' = (do' (PlaceWord move2_v) st) in
         verify_board (7, 7) st' false "sort"));

  "place_horizontal_invalid" >:: (fun _ ->
      let e = fun () -> (do' (PlaceWord move1_h_invalid) basic_state_2bag) in
      assert_raises (InvalidPlace "invalid word") e);

  "place_vertical_invalid" >:: (fun _ ->
      let e = fun () -> (do' (PlaceWord move1_v_invalid) basic_state_2bag) in
      assert_raises (InvalidPlace "invalid word") e);

  "place_custom_word" >:: (fun _ ->
      let st = do' (AddWord "sop") basic_state_2bag in
      let st' = do' (PlaceWord move_custom_word) st in
      assert_equal true (verify_board (7, 7) st' true "sop"));

  "place_prefix_horizontal" >:: (fun _ ->
      let st = (do' (PlaceWord move1_h_hat) prefix_state) in
      let st' = (do' (PlaceWord move2_h_chat) st) in
      assert_equal true (verify_board (7, 6) st' true "chat"));

  "place_prefix_vertical" >:: (fun _ ->
      let st = (do' (PlaceWord move1_v_hat) prefix_state) in
      let st' = (do' (PlaceWord move2_v_chat) st) in
      assert_equal true (verify_board (6, 7) st' false "chat"));

  "place_word_below_word" >:: (fun _ ->
      let st = (do' (PlaceWord it_mv1_h) it_no_state) in
      let st' = (do' (PlaceWord no_mv1_h_below) st) in
      assert_equal true ((verify_board (7, 7) st' true "it")
                         && (verify_board (8, 7) st' true "no")));

  "place_word_above_word" >:: (fun _ ->
      let st = (do' (PlaceWord no_mv1_h) it_no_state) in
      let st' = (do' (PlaceWord it_mv1_h_above) st) in
      assert_equal true ((verify_board (7, 7) st' true "no")
                         && (verify_board (6, 7) st' true "it")));

  "place_word_right_word" >:: (fun _ ->
      let st = (do' (PlaceWord it_mv2_v) it_no_state) in
      let st' = (do' (PlaceWord no_mv2_v_right) st) in
      assert_equal true ((verify_board (7, 7) st' false "it")
                         && (verify_board (7, 8) st' false "no")));

  "place_word_left_word" >:: (fun _ ->
      let st = (do' (PlaceWord no_mv2_v) it_no_state) in
      let st' = (do' (PlaceWord it_mv2_v_left) st) in
      assert_equal true ((verify_board (7, 6) st' false "it")
                         && (verify_board (7, 7) st' false "no")));

   (* place error check tests *)
   "place_out_of_bounds_bottom" >:: (fun _ ->
       let e = fun () -> do' (PlaceWord scrabble_vert_outofbounds) scrabble_vert_setup_st in
       assert_raises (InvalidPlace "cannot place off board") e);

   "place_out_of_bounds_right" >:: (fun _ ->
       let e = fun () -> do' (PlaceWord scrabble_hor_outofbounds) scrabble_hor_setup_st in
       assert_raises (InvalidPlace "cannot place off board") e);

   "place_not_connected_to_preexisting_board" >:: (fun _ ->
       let e = fun () -> do' (PlaceWord disconnected_mv) scrabble_hor_setup_st in
       assert_raises (InvalidPlace "not connected to board") e);

   "place_letters_not_in_rack" >:: (fun _ ->
       let e = fun () -> do' (PlaceWord no_mv1_h_below) {it_hor_st with current_player = scrabble_player} in
       assert_raises (InvalidPlace "letters not in rack") e);

   "place_invalid_second_word_vert" >:: (fun _ ->
       let e = fun () -> do' (PlaceWord {no_mv1_h_below with mv_coord=(8,8)}) it_hor_st in
       assert_raises (InvalidPlace "invalid newly-formed word") e);

   "place_invalid_second_word_hor" >:: (fun _ ->
       let e = fun () -> do' (PlaceWord {no_mv2_v_right with mv_coord=(8,8)}) it_vert_st in
       assert_raises (InvalidPlace "invalid newly-formed word") e);

   "place_not_on_center_tile" >:: (fun _ ->
       let e = fun () -> do' (PlaceWord {no_mv2_v_right with mv_coord=(8,8)}) blank1_state in
       assert_raises (InvalidPlace "must fill center tile") e);

  (* sp_consec tests *)
  "place_sp_consec" >:: (fun _ ->
      let st = (do' (PlaceWord no_mv2_v) it_no_state) in
      assert_equal 0 st.sp_consec);

  "sp_consec_reset" >:: (fun _ ->
      let st = (do' (Swap ['i']) it_no_state) in
      let st' = (do' (PlaceWord no_mv2_v) st) in
      assert_equal true (st'.sp_consec = 0 && st.sp_consec = 1));
]

let pass_tests = [
  (* pass tests *)
  "pass_next_player" >:: (fun _ ->
      let st = (do' Pass basic_state_2bag) in
      assert_equal 2 st.current_player.order_num);

  "pass_sp_consec" >:: (fun _ ->
      let st = (do' Pass basic_state_2bag) in
      assert_equal 1 st.sp_consec);
]
