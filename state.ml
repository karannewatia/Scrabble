open Trie
open Command

type coordinate = int * int

type letter = (char * int)

type cell = {
  cell_coord : coordinate;
  letter : letter;
  letter_multiplier : int;
  word_multiplier : int;
}

type board = (cell list) list

type bag = letter list

type difficulty = Easy | Hard

type player_type = Human | AI of difficulty

type player = {
  name : string;
  score: int;
  rack : letter list;
  player_type : player_type;
  order_num : int;
}

type init_game_data = {
  num_players : int;
  num_humans : int;
  ai_difficulty : difficulty list;
  human_names : string list;
}

type state = {
  board : board;
  bag : bag;
  players : player list;
  added_words : string list;
  current_player : player;
  sp_consec : int;
  is_first_move : bool;
}

(* [get_points c] returns the number of points associated with letter [c]. *)
let get_points c =
  match c with
  | 'a' | 'e' | 'i' | 'l' | 'n'
  | 'o' | 'r' | 's' | 't' | 'u'
  | 'A' | 'E' | 'I' | 'L' | 'N'
  | 'O' | 'R' | 'S' | 'T' | 'U' -> 1
  | 'd' | 'g' | 'D' | 'G'       -> 2
  | 'b' | 'c' | 'm' | 'p'
  | 'B' | 'C' | 'M' | 'P'       -> 3
  | 'f' | 'h' | 'v' | 'w' | 'y'
  | 'F' | 'H' | 'V' | 'W' | 'Y' -> 4
  | 'k' | 'K'                   -> 5
  | 'j' | 'x' | 'J' | 'X'       -> 8
  | 'q' | 'z' | 'Q' | 'Z'       -> 10
  | '*'                         -> 0
  | _ -> failwith "impossible"

(* [get_multipliers coord] is the (letter multiplier, word multiplier) pair
 * representing the multipliers for a given [coord]. *)
let get_multipliers coord =
  match coord with
  | (1,1)  | (2,2)   | (3,3)  | (4,4)
  | (1,13) | (2,12)  | (3,11) | (4,10)
  | (10,4) | (10,10) | (11,3) | (11,11)
  | (12,2) | (12,12) | (13,1) | (13,13)
  | (7,7)                               -> (1,2)
  | (0,0)  | (0,7)   | (0,14) | (7,0)
  | (7,14) | (14,0)  | (14,7) | (14,14) -> (1,3)
  | (3,0)  | (11,0)  | (6,2)  | (7,3)
  | (8,2)  | (0,3)   | (14,3) | (2,6)
  | (6,6)  | (8,6)   | (12,6) | (3,7)
  | (11,7) | (2,8)   | (6,8)  | (8,8)
  | (12,8) | (0,11)  | (7,11) | (14,11)
  | (6,12) | (8,12)  | (3,14) | (11,14) -> (2,1)
  | (5,1)  | (9,1)   | (1,5)  | (5,5)
  | (9,5)  | (13,5)  | (1,9)  | (5,9)
  | (9,9)  | (13,9)  | (5,13) | (9,13)  -> (3,1)
  | _                                   -> (1,1)

(* [init_board n] creates an nxn board.
 * requires: n > 0 *)
let rec init_board n =
  let rec helper n' =
    match n' with
    | 0 -> gen_row 0 n :: []
    | i -> helper (i - 1) @ [gen_row i n] in
  helper (n - 1)
and gen_row row_num len =
  match len with
  | 0 -> []
  | i -> gen_row row_num (len - 1) @ [gen_cell row_num (i-1)]
and gen_cell row_num col_num =
  {cell_coord = row_num, col_num;
   letter = (' ', -1);
   letter_multiplier = fst (get_multipliers (row_num, col_num));
   word_multiplier = snd (get_multipliers (row_num, col_num))}

(* [init_bag ()] creates a scrabble bag of tiles *)
let rec init_bag () =
  let alphabet = ['a';'b';'c';'d';'e';'f';'g';'h';'i';'j';'k';'l';'m';'n';'o';
                  'p';'q';'r';'s';'t';'u';'v';'w';'x';'y';'z';'*'] in
  List.flatten (List.map init_letters_of_char alphabet)
and init_letters_of_char c =
  match c with
| 'a' -> num_tiles_of_char 9 'a'   | 'b' -> num_tiles_of_char 2 'b'
| 'c' -> num_tiles_of_char 2 'c'   | 'd' -> num_tiles_of_char 4 'd'
| 'e' -> num_tiles_of_char 12 'e'  | 'f' -> num_tiles_of_char 2 'f'
| 'g' -> num_tiles_of_char 3 'g'   | 'h' -> num_tiles_of_char 2 'h'
| 'i' -> num_tiles_of_char 9 'i'   | 'j' -> num_tiles_of_char 1 'j'
| 'k' -> num_tiles_of_char 1 'k'   | 'l' -> num_tiles_of_char 4 'l'
| 'm' -> num_tiles_of_char 2 'm'   | 'n' -> num_tiles_of_char 6 'n'
| 'o' -> num_tiles_of_char 8 'o'   | 'p' -> num_tiles_of_char 2 'p'
| 'q' -> num_tiles_of_char 1 'q'   | 'r' -> num_tiles_of_char 6 'r'
| 's' -> num_tiles_of_char 4 's'   | 't' -> num_tiles_of_char 6 't'
| 'u' -> num_tiles_of_char 4 'u'   | 'v' -> num_tiles_of_char 2 'v'
| 'w' -> num_tiles_of_char 2 'w'   | 'x' -> num_tiles_of_char 1 'x'
| 'y' -> num_tiles_of_char 2 'y'   | 'z' -> num_tiles_of_char 1 'z'
| '*' -> num_tiles_of_char 2 '*'   | _ -> failwith "impossible"
and num_tiles_of_char n c =
  let p = get_points c in
  match n with
  | 0 -> []
  | i -> (c, p) :: num_tiles_of_char (n - 1) c

(* [gen_human_players num names] is a list of human players *)
let rec gen_human_players num names =
  match num, names with
  | _, [] -> []
  | i, name::t -> {name = name; score = 0; rack = [];
                   player_type = Human; order_num = i}
                  :: gen_human_players (num + 1) t

(* [gen_ai_players num_ai difficulty] is a list of ai players *)
let rec gen_ai_players num_players num_ai difficulty =
  match num_ai, difficulty with
  | _, [] -> []
  | i, diff::t ->
    {name = "AI_"^(string_of_int (num_players - i + 1)); score = 0; rack = [];
     player_type = AI diff; order_num = (num_players-i+1)}
    :: gen_ai_players num_players (num_ai - 1) t

(* [init_players init_data] is the initial players list (both human and AI) *)
let init_players init_data =
  let num_players = init_data.num_players in
  let num_humans = init_data.num_humans in
  let human_players = gen_human_players 1 init_data.human_names in
  let ai_players =
    gen_ai_players num_players (num_players - num_humans)
      init_data.ai_difficulty in
  human_players @ ai_players

(* [update_rack_and_bag chars_from_rack rack bag] adds letters from [bag] to
 * [rack] until the player has 7 letters on their rack.
 * returns: a pair with the updated [rack] and [bag]. *)
let update_rack_and_bag chars_from_rack rack bag =
  let rack' =
    List.fold_left
      (fun acc c -> List.remove_assoc c acc) rack chars_from_rack in
  let rec update_rack r b =
    if List.length r = 7 || List.length b = 0 then
      (r, b)
    else
      begin

        (* Random.self_init (); *)
        Random.init 35;

        let i = Random.int (List.length b) in
        let letter_from_bag = List.nth b i in
        let updated_bag = List.remove_assoc (fst letter_from_bag) b in
        update_rack (letter_from_bag :: r) updated_bag
      end
  in
  update_rack rack' bag

(* [update_players current_player rack players new_points] is the updated
 * players list with [current_player] getting [new_points] added to their score,
 * and [rack] assigned as their rack field. *)
let update_players current_player rack players new_points =
  let updated_player =
    {current_player with rack = rack;
                         score = current_player.score + new_points} in
  (List.filter
     (fun p -> p.name <> updated_player.name) players) @ [updated_player]

(* [init_racks players st] is the updated state with players having been
 * assigned initial racks *)
let rec init_racks players st =
  match players with
  | [] -> st
  | h::t ->
    let rack_bag = update_rack_and_bag [] h.rack st.bag in
    let updated_players =
      update_players h (fst rack_bag) (st.players) 0 in
    let st' = {st with bag = (snd rack_bag); players = updated_players} in
    init_racks t st'

(* [init_state init_data] is the initial game state based off [init_data] *)
let init_state init_data =
  let board = init_board 15 in
  let bag = init_bag () in
  let players = init_players init_data in
  let current_player = List.hd players in
  let st = {board = board;
            bag = bag;
            players = players;
            added_words = [];
            current_player = current_player;
            sp_consec = 0;
            is_first_move = true;
           } in
  let st' = init_racks players st in
  {st' with current_player = List.hd st'.players}

exception InvalidPlace of string

exception InvalidSwap

exception InvalidAdd

(* dictionary *)
let dict = Trie.initialize_dict "forward_dict.txt"

(* [get_row c st] returns a list of cells representing the row that coordinate
 * [c] lies in.
 * raises: [InvalidPlace] if (fst c) is greater than the number of rows. *)
let get_row c st =
  try List.nth st.board (fst c) with
  | exn -> raise (InvalidPlace "invalid row coord")

(* [get_column c st] returns a list of cells representing the column that
 * coordinate [c] lies in.
 * raises: [InvalidPlace] if (snd c) is greater than the number of columns. *)
let get_column c st =
  let col_num = snd c in
  try
    let rec col_helper b acc =
      match b with
      | [] -> acc
      | row::t -> col_helper t (List.nth row col_num :: acc)
    in List.rev (col_helper st.board [])
  with
  | exn -> raise (InvalidPlace "invalid col coord")

(* [get_cell_from_coordinate c st] returns the cell at coordinate [c] on the
 * board in [st].
 * raises: [InvalidPlace] if [c] does not correspond to a cell in the board. *)
let get_cell_from_coordinate c st =
  try
    let row = get_row c st in
    List.nth row (snd c)
  with
  | exn -> raise (InvalidPlace "invalid cell coord")

(* [cell_is_empty c] returns [true] if cell [c] is empty and returns [false] if
 * there is a [letter] at cell [c]. *)
let cell_is_empty c =
  c.letter = (' ', -1)

(* [fst_triple t] returns the first element from tuple [t] with three
 * elements. *)
let fst_triple t =
  match t with
  | (x, _, _) -> x

(* [snd_triple t] returns the second element from tuple [t] with three
 * elements. *)
let snd_triple t =
  match t with
  | (_, x, _) -> x

(* [trd_triple t] returns the third element from tuple [t] with three
 * elements. *)
let trd_triple t =
  match t with
  | (_, _, x) -> x

(* [get_adjacent_cells c st is_h] returns a list of cells that contains the
 * adjacent word formed by placing a letter at coordinate [c]. [is_h] determines
 * whether the adjacent word is searched for horizontally or vertically. *)
let get_adjacent_cells c st is_h =
  let cell = get_cell_from_coordinate c st in
  let cell_list =
    if is_h then
      get_row c st
    else
      get_column c st
  in
  let rec get_correct_word lst acc =
    match lst with
    | [] -> acc
    | h::t ->
      begin
        match h.letter with
        | (' ', _) ->
          if List.mem cell acc then
            acc
          else
            get_correct_word t []
        | (x, _) -> get_correct_word t (h :: acc)
      end
  in List.rev (get_correct_word cell_list [])

(* [has_adj_new_chars c is_h st] returns true if the cell at coordinate [c] has
 * a non-empty cell adjacent to it in the direction specified by [is_h]. If
 * the cells adjacent to [c] are empty or [c] is empty, then false is returned.
*)
let has_adj_new_chars c is_h st =
  if cell_is_empty (get_cell_from_coordinate c st) then
    false
  else
    begin
      match (is_h, c) with
      | (false, (0,_)) ->
        let cell_below =
          get_cell_from_coordinate (fst c + 1, snd c) st in
        cell_below.letter <> (' ', -1)
      | (false, (14, _)) ->
        let cell_above =
          get_cell_from_coordinate (fst c - 1, snd c) st in
        cell_above.letter <> (' ', -1)
      | (false, (r_idx, c_idx)) ->
        let cell_below =
          get_cell_from_coordinate (r_idx + 1, c_idx) st in
        let cell_above =
          get_cell_from_coordinate (r_idx - 1, c_idx) st in
        cell_below.letter <> (' ', -1) || cell_above.letter <> (' ', -1)
      | (true, (_, 0)) ->
        let cell_right =
          get_cell_from_coordinate (fst c, snd c + 1) st in
        cell_right.letter <> (' ', -1)
      | (true, (_, 14)) ->
        let cell_left =
          get_cell_from_coordinate (fst c, snd c - 1) st in
        cell_left.letter <> (' ', -1)
      | (true, (r_idx, c_idx)) ->
        let cell_right =
          get_cell_from_coordinate (r_idx, c_idx + 1) st in
        let cell_left =
          get_cell_from_coordinate (r_idx, c_idx - 1) st in
        cell_right.letter <> (' ', -1) || cell_left.letter <> (' ', -1)
    end

(* [get_adjacent_word c st is_h new_chars] returns a triple option with the
 * adjacent word at coordinate [c] on the board in [st], the points
 * associated with it, and the word multiplier applied to it, used later for
 * correcting blank tile scoring. Only coordinates in [new_coord] have their
 * scores multiplied by their letter multiplier. [is_h] determines whether the
 * adjacent word is searched for horizontally or vertically. None is returned if
 * [c] is empty or there are no adjacent characters to [c] in the direction
 * specified by [is_h]. *)
let get_adjacent_word c st is_h new_coords =
  if has_adj_new_chars c is_h st then
    let word_cells = get_adjacent_cells c st is_h in
    let rec adjacent_helper lst (acc : (string * int * int)) =
      match lst with
      | [] -> acc
      | h::t ->
        let new_string = (fst_triple acc) ^ (Char.escaped (fst h.letter)) in
        let points =
          if List.mem h.cell_coord new_coords then
            (snd_triple acc) + (h.letter_multiplier * (snd h.letter))
          else
            (snd_triple acc) + (snd h.letter)
        in
        let word_multiplier =
          if List.mem h.cell_coord new_coords then
            (trd_triple acc) * h.word_multiplier
          else
            trd_triple acc
        in
        adjacent_helper t (new_string, points, word_multiplier)
    in
    let word_triple = adjacent_helper word_cells ("", 0, 1) in
    Some (fst_triple word_triple,
          (snd_triple word_triple) * (trd_triple word_triple),
          trd_triple word_triple)
  else
    None

(* [get_next_player n p] returns the player whose turn is next given the
 * current player's order number [n] and a list of the players [p]. *)
let rec get_next_player n p =
  let n' =
    if n = List.length p then 1
    else n + 1
  in
  List.hd (List.filter (fun p -> p.order_num = n') p)

(* [place_horizontal mv st] returns the updated board after the word in [mv] is
 * placed horizontally on the board in [st]. *)
let place_horizontal mv st =
  let row_idx = fst mv.mv_coord in
  let rec helper board count =
    match board with
    | [] -> []
    | row::t ->
      if count <> row_idx then
        row :: helper t (count+1)
      else
        (* row corresponds to row to which we want to add chars *)
        let rec helper' row count =
          match row with
          | [] -> []
          | cell::t ->
            if count < (snd mv.mv_coord) ||
               count >= (snd mv.mv_coord + (List.length mv.word)) then
              cell :: helper' t (count+1)
            else (* cell letter needs to be updated *)
              let letter_char = List.nth mv.word (count - (snd mv.mv_coord)) in
              let letter_points = get_points letter_char in
              {cell with letter =
                           (letter_char, letter_points)} :: helper' t (count+1)
        in helper' row 0 :: helper t (count+1)
  in helper st.board 0

(* [place_vertical mv st] returns the updated board after the word in [mv] is
 * placed vertically on the board in [st]. *)
let place_vertical mv st =
  let row_idx = fst mv.mv_coord in
  let col_idx = snd mv.mv_coord in
  let rec helper board acc =
    match board with
    | [] -> acc
    | row::t ->
      let cell = List.nth row col_idx in
      if fst cell.cell_coord < row_idx
      || fst cell.cell_coord >= (row_idx + (List.length mv.word)) then
        helper t (row :: acc)
      else
        let letter_char = List.nth mv.word (fst cell.cell_coord - row_idx) in
        let letter_points = get_points letter_char in
        let updated_cell = {cell with letter = (letter_char, letter_points)} in
        let updated_row =
          let rec update_row lst acc =
            match lst with
            | [] -> acc
            | h::t ->
              if h.cell_coord = updated_cell.cell_coord then
                update_row t (updated_cell :: acc)
              else
                update_row t (h :: acc)
          in update_row row []
        in helper t ((List.rev updated_row) :: acc)
  in List.rev (helper st.board [])

(* [check_word word st] returns true if [word] is a valid word in dict or in the
 * list of added words, otherwise returns false. *)
let check_word word st =
  (Trie.is_word dict word) || (List.mem word st.added_words)

(* [check_bounds mv st] is true if [mv.word] fits on the board *)
let check_bounds mv st =
  let word = List.fold_right (fun c acc -> (Char.escaped c)^acc) mv.word "" in
  if mv.is_horizontal then (snd mv.mv_coord) + String.length word <= 15
  else (fst mv.mv_coord) + String.length word <= 15

(* [check_endpoints mv st] is true if the cell to left/top and cell to
 * right/botto (depending on if mv is horizontal or vertical) is either empty
 * or nonexistant. *)
let check_endpoints mv st =
  let r = fst mv.mv_coord in
  let c = snd mv.mv_coord in
  if mv.is_horizontal then
    let left_empty =
      try get_cell_from_coordinate (r, c - 1) st |> cell_is_empty
      with InvalidPlace _ -> true in
    let right_empty =
      try get_cell_from_coordinate (r, c + (List.length mv.word)) st
          |> cell_is_empty
      with InvalidPlace _ -> true in
    left_empty && right_empty
  else
    let top_empty =
      try get_cell_from_coordinate (r - 1, c) st |> cell_is_empty
      with InvalidPlace _ -> true in
    let bottom_empty =
      try get_cell_from_coordinate (r + (List.length mv.word), c) st
          |> cell_is_empty
      with InvalidPlace _ -> true in
    top_empty && bottom_empty

(* [check_fit_and_new_entries mv st] is the list of new chars and their
 * respective coordinates on the board, assuming mv.word doesn't violate the
 * current board state.
 * raises: InvalidPlace when mv.word violates current board state. *)
let check_fit_and_new_entries mv st =
  if mv.is_horizontal then
    let row = get_row mv.mv_coord st in
    let rec helper row count acc =
      match row with
      | [] -> acc
      | cell::t ->
        if count < (snd mv.mv_coord) ||
           count >= (snd mv.mv_coord + (List.length mv.word)) then
          helper t (count+1) acc
        else (* cell contains piece of new word *)
        if fst cell.letter = (List.nth mv.word (count - (snd mv.mv_coord))) then
          (* pre-existing letter, not coming from player rack *)
          helper t (count+1) acc
        else
        if fst cell.letter <> ' '
        then raise (InvalidPlace "overlapping letters")
        else (* adding char from rack to this cell *)
          helper t (count+1)
            ((List.nth mv.word (count - (snd mv.mv_coord)),
              (fst mv.mv_coord, count)) :: acc)
    in helper row 0 []
  else
    let col = get_column mv.mv_coord st in
    let rec helper col count acc =
      match col with
      | [] -> acc
      | cell::t ->
        if count < (fst mv.mv_coord) ||
           count >= (fst mv.mv_coord + (List.length mv.word)) then
          helper t (count+1) acc
        else (* cell contains piece of new word *)
        if fst cell.letter = (List.nth mv.word (count - (fst mv.mv_coord))) then
          (* pre-existing letter, not coming from player rack *)
          helper t (count+1) acc
        else
        if fst cell.letter <> ' ' then
          raise (InvalidPlace "overlapping letters")
        else (* adding char from rack to this cell *)
          helper t (count+1)
            ((List.nth mv.word (count - (fst mv.mv_coord)),
              (count, snd mv.mv_coord)) :: acc)
    in
    helper col 0 []

(* [remove c lst] returns lst with the first instance of [c] removed. If [c] is
 * not in [lst], [lst] is returned. *)
let rec remove c lst =
  match lst with
  | [] -> []
  | h::t -> if h = c then t
    else h :: remove c t

(* [check_rack rack new_board_chars] checks that new_board_chars
 * is a subset of rack *)
let check_rack rack new_board_chars =
  let board_chars = List.map (fun (c, coord) -> c) new_board_chars in
  let rack' = List.map (fun (c,p) -> c) rack in
  if List.length rack < List.length board_chars then false
  else
    let rec helper r new_board_chars =
      match new_board_chars with
      | [] -> true
      | c::t ->
        if List.mem c r && List.mem c board_chars then
          true && helper (remove c r) t
        else if List.mem '*' r && List.mem c board_chars then
          true && helper (remove '*' r) t
        else false
    in helper rack' board_chars

(* [update_board mv st] is the updated board based off [mv] and [st]. *)
let update_board mv st =
  if mv.is_horizontal then
    place_horizontal mv st
  else place_vertical mv st

(* [get_values_from_opt_list opt_lst acc] returns a list containing the
 * extracted values from [opt_lst]. [None] doesn't add anything to the returned
 * list. *)
let rec get_values_from_opt_list opt_lst acc =
  match opt_lst with
  | [] -> acc
  | (Some p)::t -> get_values_from_opt_list t (p :: acc)
  | (None)::t -> get_values_from_opt_list t acc

(* [refresh_rack new_chars rack] swaps blank tiles in [rack] with the subset of
 * letters in new_chars that are not explicitly present in the rack. *)
let refresh_rack new_chars rack =
  let new_chars' = List.map (fun (c,coord) -> c) new_chars in
  let rack_chars = List.map (fun (c,_) -> c) rack in
  let rec helper chars r =
    match chars with
    | [] -> []
    | h::t ->
      if List.mem h r then
        helper t r
      else
        h :: (helper t r)
  in
  let chars_represented_by_blanks = helper new_chars' rack_chars in
  let rack_without_blanks =
    if List.length chars_represented_by_blanks = 0 then rack_chars
    else if List.length chars_represented_by_blanks = 1
    then (remove '*' rack_chars)
    else
      remove '*' (remove '*' rack_chars)
  in
  let fixed_chars = List.map (fun c -> (c, 0)) chars_represented_by_blanks in
  let fixed_rack = List.map (fun c -> (c, get_points c)) rack_without_blanks in
  fixed_rack @ fixed_chars

(* [fix_score curr_player players rack'] is the list of players, with the current
 * player's ([curr_player]) score adjusted for any possible use
 * of blank tiles *)
let fix_score curr_player players rack' word_mult =
  let rec helper r =
    match r with
    | [] -> 0
    | (c,0)::t ->
      if c <> '*' then (word_mult * get_points c) + (helper t) else helper t
    | _::t -> helper t in
  let points_to_deduct = helper rack' in
  let p = List.hd (List.filter (fun p -> p.name = curr_player) players) in
  {p with score = p.score - points_to_deduct} ::
  (List.filter (fun p -> p.name <> curr_player) players)

(* [place w c is_h] places word segment [w] at coordinate [c] horizontally if
 * [is_h] is true and vertically if [is_h] is false.
 * raises: [InvalidPlace] if one cannot place [w] at coordinate [c]. *)
let rec place mv st =
  let word = List.fold_right (fun c acc -> (Char.escaped c)^acc) mv.word "" in
  if not (check_word word st) then raise (InvalidPlace "invalid word")
  else if not (check_bounds mv st) then
    raise (InvalidPlace "cannot place off board")
  else if not (check_endpoints mv st) then
    raise (InvalidPlace "not complete word")
  else
    (* check first-move-of-game condition *)
  if st.is_first_move then
    (* if List.for_all (fun p -> p.score = 0) st.players then *)
    let new_chars = check_fit_and_new_entries mv st in
    if not (check_rack st.current_player.rack new_chars) then
      raise (InvalidPlace "letters not in rack")
    else
      let rack' = refresh_rack new_chars st.current_player.rack in
      let board' = update_board mv st in
      let row7 = List.nth board' 7 in
      let cell7 = List.nth row7 7 in
      if cell_is_empty cell7 then
        raise (InvalidPlace "must fill center tile")
      else (* word was placed on center tile *)
        (* update score, player rack, current player, bag *)
        let current_player = st.current_player in
        let rack_bag = update_rack_and_bag mv.word rack' st.bag in
        let new_coords = List.map (fun (_,coord) -> coord) new_chars in
        let word_score_opt =
          get_adjacent_word mv.mv_coord {st with board = board'}
            mv.is_horizontal new_coords in
        let word_score =
          List.hd (get_values_from_opt_list [word_score_opt] []) in
        let score' = (* score adjusted for bingo rule *)
          if List.length new_chars = 7 then
            (snd_triple word_score + 50) else (snd_triple word_score) in
        let updated_players =
          update_players current_player (fst rack_bag) st.players score' in
        let updated_players' =
          fix_score current_player.name updated_players rack'
            (trd_triple word_score) in
        let next_player =
          get_next_player current_player.order_num updated_players' in
        {st with players = updated_players';
                 board = board';
                 current_player = next_player;
                 bag = (snd rack_bag);
                 sp_consec = 0;
                 is_first_move = false;
        }
  else (* not first move of game *)
    (* new_chars is an assoc list of character*coord *)
    let new_chars = check_fit_and_new_entries mv st in
    if not (check_rack st.current_player.rack new_chars) then
      (* newly-placed chars not in player rack *)
      raise (InvalidPlace "letters not in rack")
    else
      (* assuming place is valid... *)
      let rack' = refresh_rack new_chars st.current_player.rack in
      let board' = update_board mv st in
      let just_new_chars = List.map (fun (c,coord) -> c) new_chars in
      let new_coords = List.map (fun (_,coord) -> coord) new_chars in
      let current_player = st.current_player in
      let rack_bag = update_rack_and_bag just_new_chars rack' st.bag in
      let st_board = {st with board = board'} in
      if List.length new_chars = List.length mv.word &&
         not (List.fold_left
                (fun acc (_, c) ->
                   (has_adj_new_chars c (not mv.is_horizontal) st_board) || acc)
                false new_chars) then
        raise (InvalidPlace "not connected to board")
      else
        let word_score_opp_dir_opt =
          List.fold_left (fun acc c ->
              (get_adjacent_word (snd c) st_board
                 (not mv.is_horizontal) new_coords) :: acc) [] new_chars in
        let word_score_lst_opt =
          (get_adjacent_word mv.mv_coord st_board mv.is_horizontal new_coords)
          :: word_score_opp_dir_opt in
        let word_score_lst = get_values_from_opt_list word_score_lst_opt [] in
        let valid_words =
          List.fold_left (fun acc (s, i, wm) ->
              (fst_triple acc &&
               check_word s st, snd_triple acc + i, trd_triple acc * wm))
            (true, 0, 1) word_score_lst in
        let score' = (* score adjusted for bingo rule *)
          if List.length new_chars = 7 then (snd_triple valid_words + 50)
          else (snd_triple valid_words) in
        if fst_triple valid_words then
          let updated_players =
            update_players current_player (fst rack_bag) st.players score' in
          let updated_players' =
            fix_score current_player.name updated_players rack'
              (trd_triple valid_words) in
          let next_player =
            get_next_player current_player.order_num updated_players' in
          {st_board with players = updated_players';
                         current_player = next_player;
                         bag = (snd rack_bag);
                         sp_consec = 0}
        else
          raise (InvalidPlace "invalid newly-formed word")

(* [swap lst st] removes the letters in [lst] from the current player's rack and
 * swaps them with letters in the bag.
 * returns: the updated [state] after the swap.
 * raises: [InvalidSwap] if a letter in [lst] is not in the current
 * player's rack or there are not enough letters in the bag to do the swap. *)
let swap lst st =
  let player = st.current_player in
  let valid_swap = List.for_all (fun (x) -> List.mem_assoc x player.rack) lst in
  if (List.length st.bag >= List.length lst) && valid_swap then
    let rack_bag = update_rack_and_bag lst player.rack st.bag in
    let players' = update_players player (fst rack_bag) st.players 0 in
    let st' = {st with players = players'; bag = (snd rack_bag)} in
    let updated_current_player = get_next_player player.order_num st'.players in
    let rec add_to_bag l b =
      match l with
      | [] -> b
      | h::t -> add_to_bag t ((h, get_points h) :: b)
    in let updated_bag = add_to_bag lst st'.bag in
    {st' with bag = updated_bag;
              current_player = updated_current_player;
              sp_consec = st.sp_consec + 1}
  else
    raise InvalidSwap

(* [pass st] returns [st] with the sp_consec field incremented by 1. *)
let pass st =
  {st with current_player =
             get_next_player st.current_player.order_num st.players;
             sp_consec = st.sp_consec + 1}

(* [check_word s] ensures that [s] only contains characters in the alphabet *)
let check_word_chars s =
  let splt = Str.split (Str.regexp "[^a-zA-Z]") s in
  if List.length splt = 1 then s
  else raise InvalidAdd

(* [add_word word st] adds [word] to the [added_words] field of [st] and returns
 * the updated state. *)
let add_word word st =
  let word' = check_word_chars word in
  let added_words' = word'::st.added_words in
  {st with added_words = added_words'}

let do' cmd st =
  match cmd with
  | Swap lst -> swap lst st
  | AddWord s -> add_word s st
  | PlaceWord mv -> place mv st
  | Pass -> pass st
  | _ -> st
