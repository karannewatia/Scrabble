open State
open Command
open Char
open Gui

(* [get_scores players] returns a string of player, score information *)
let rec get_scores players =
  let string_of_score p = p.name ^ ": " ^ (string_of_int p.score) in
  let score_string_list = (List.map string_of_score (List.rev players)) in
  String.concat "\n" score_string_list

(* [get_prev_player n p] returns the player whose turn was before the
 * current player with order number [n] given a list of the players [p]. *)
let rec get_prev_player n p =
  let n' =
    if n = 1 then List.length p
    else n - 1
  in
  List.hd (List.filter (fun p -> p.order_num = n') p)

(*[finalize_scores players] is the list of players, with their respective scores
 * updated based on the tiles left in their rack. Each player's score is reduced
 * by the sum of their unplayed letters. If a player played all their letters,
 * the sum of the other players' unplayed letters is added to thta player's
 * score. *)
let finalize_scores players =
  let empty_rack_player =
    if List.length (List.filter (fun p -> List.length p.rack = 0) players) <> 0
    then
      Some (List.hd (List.filter (fun p -> List.length p.rack = 0) players))
    else None in
  let other_players = List.filter (fun p -> List.length p.rack <> 0) players in
  let score_helper s r =
    List.fold_left (fun acc (l,p) -> acc - p) s r in
  let other_players'_w_sumDeductedPts =
    List.fold_left (fun acc p ->
        let deducted_ps = p.score - score_helper p.score p.rack in
        ({p with score = p.score - deducted_ps}::(fst acc)),
        snd acc + deducted_ps) ([],0) other_players in
  match empty_rack_player with
  | Some p ->
    let empty_rack_player' =
      {p with score = p.score + snd other_players'_w_sumDeductedPts} in
    empty_rack_player' :: fst other_players'_w_sumDeductedPts
  | None -> fst other_players'_w_sumDeductedPts

(* [get_winner st] is the player with the highest score *)
let get_winner st =
  let players' = finalize_scores st.players in
  let winner_s = List.fold_left
      (fun acc p ->
         if p.score > (List.hd acc).score then [p]
         else
         if p.score = (List.hd acc).score then
           p::acc
         else acc) [List.hd players'] players' in
  if List.length winner_s = 1 then List.hd winner_s
  else List.fold_left (fun acc p ->
      if p.score > acc.score then p else acc) st.current_player st.players

(* [no_empty_rack st] is true if no player has an empty rack *)
let no_empty_rack st =
  List.for_all (fun p -> List.length p.rack <> 0) st.players

(* [end_nonturn_command str] prints some output [str] to the gui and prompts the
 * user to press any key to continue their turn *)
let end_nonturn_command str =
  erase_io_box ();
  Graphics.set_color Graphics.black;
  Graphics.moveto 625 240;
  Graphics.draw_string str;
  Graphics.moveto 625 220;
  Graphics.draw_string "Press any key to continue your turn";
  let _ = Graphics.wait_next_event [Graphics.Key_pressed] in ()

(* [get_command st] is a command, generated from user input *)
let rec get_command st =
  match st.current_player.player_type with
  | Human ->
    (try Gui.gui_cmd st with
     | GuiExn s -> end_nonturn_command ("Exception: " ^ s);
       erase_io_box ();
       Gui.update_gui `Place st;
       get_command st)
  | AI diff ->
    try
    match diff with
    | Hard -> Ai.best_move st
    | Easy -> Ai.get_hint st
    with
    | InvalidSwap | InvalidPlace _ -> Pass
    |  _ ->
      Graphics.close_graph ();
      exit 0

(* [quit_helper st] acts as a second step of verification for a Quit command *)
let quit_helper st =
  Graphics.set_color Graphics.black;
  Graphics.moveto 625 240;
  Graphics.draw_string
    "Press 'Q' to confirm quit, any other key to resume play";
  let s = Graphics.wait_next_event [Graphics.Key_pressed] in
  match Char.code s.Graphics.key with
  | 81 | 113 -> print_endline "\nThanks for playing!"; exit 0
  | _ -> st

(* [hint_helper st] displays a hint to the user and returns [st] *)
let hint_helper st =
  let hint =
    match (Ai.get_hint st) with
    | PlaceWord mv ->
      "Hint: " ^ List.fold_right
        (fun x acc -> (Char.escaped x) ^ acc) mv.word ""
    | _ -> "Hint: you should swap or pass" in
  end_nonturn_command hint;
  st

(* [play_game st] plays the game represented by [st]. *)
let rec play_game st =
  let command = get_command st in
  let new_state =
    try
      match command with
      | PlaceWord _ ->
        let st' = do' command st in Gui.update_gui `Place st'; st'
      | Pass -> let st' = do' command st in Gui.update_gui `Pass st'; st'
      | Swap _ -> let st' = do' command st in Gui.update_gui `Swap st'; st'
      | Rack -> st
      | Help -> Gui.update_gui `Help st; st
      | Hint -> hint_helper st
      | AddWord str ->
        let st' = do' command st in end_nonturn_command
          ("Added word to dictionary"); st'
      | Quit -> quit_helper st
    with
    | InvalidPlace s ->
      end_nonturn_command ("Invalid Place: " ^ s); Gui.update_gui `Place st; st
    | InvalidSwap ->
      (end_nonturn_command
         "Cannot swap, bag is empty or doesn't have enough tiles";
         Gui.update_gui `Swap st; st)
    | InvalidAdd -> end_nonturn_command ("Invalid Add"); st
  in
  erase_io_box ();
  if st.current_player.name <> new_state.current_player.name then
    (erase_rack ();
     erase_toggle_button ();
     let b_toggle_rack = {x = 632; y = 120; w = 60; h = 60; bw = 2;
                          b1_col = gray1; b2_col = gray3;
                          b_col = gray2} in
     draw_box b_toggle_rack;
     draw_string_in_box "Show rack" b_toggle_rack Graphics.black);
  let num_players = List.length st.players in
  if no_empty_rack new_state && new_state.sp_consec < (2*num_players) then
    play_game new_state
  else
    (let winner = (get_winner new_state).name in
     Graphics.moveto 625 235;
     Graphics.draw_string ("Congratuations, " ^ winner ^ ", you win!");
     Graphics.moveto 625 220;
     Graphics.draw_string "Press any key to quit";
     let _ = Graphics.wait_next_event [Graphics.Key_pressed] in
     exit 0;)

(******************************************************************************)
(* GAME INITIALIZATION CODE *)

(* [draw_init_game_logo ()] draws 'scrabble' at the top of the game initialization
 * gui window. *)
let draw_init_game_logo () =
  let bar_height = snd (Graphics.text_size "|") in
  draw_string "                               _       _       _         " 25 375 true;
  draw_string "  ___    ___    _ __    __ _  | |__   | |__   | |   ___  " 25 (375 - bar_height) true;
  draw_string " / __|  / __|  | '__| / _` | |  _ \\ |  _ \\ | |  / _ \\ " 25 (375 - 2 * bar_height) true;
  draw_string " \\__\\| (__   | |    | (_| | | |_) | | |_) | | | |  __/ " 25 (375 - 3 * bar_height) true;
  draw_string " |___/  \\___| |_|    \\__,_| |_.__/  |_.__/  |_|  \\___|" 25 (375 - 4 * bar_height) true;
  draw_string " ________________________________________________________" 25 (375 - 5 * bar_height) true

(* [reset ()] resets the game initialization window to its
 * default configuration *)
let reset () =
  Graphics.clear_graph ();
  draw_init_game_logo ();
  Graphics.draw_rect 20 20 360 240;
  Graphics.moveto 20 290;
  Graphics.draw_string "Welcome to Scrabble!";
  Graphics.moveto 20 275;
  Graphics.draw_string "Made by A. Vaziri, C. McHugh, D. Lim, K. Newatia"

(* [clear_init_io_box ()] clears the initial game io box *)
let clear_init_io_box () =
  Graphics.set_color Graphics.white;
  Graphics.fill_rect 21 21 358 238;
  Graphics.set_color Graphics.black

(* [inp_reset ()] resets the text box to the default prompt for
 * init_num_players *)
let inp_reset () =
  clear_init_io_box ();
  Graphics.moveto 30 240;
  Graphics.draw_string "How many players? Enter a number between 1 and 4,";
  Graphics.moveto 30 225;
  Graphics.draw_string "followed by 'ENTER'";
  Graphics.moveto 30 210;
  Graphics.draw_string "> "

(* [inh_reset ()] resets the text box to the default prompt for
 * init_num_humans *)
let inh_reset () =
  clear_init_io_box ();
  Graphics.moveto 30 240;
  Graphics.draw_string "How many human players? Enter a number, followed";
  Graphics.moveto 30 225;
  Graphics.draw_string "by 'ENTER'";
  Graphics.moveto 30 210;
  Graphics.draw_string "> "

(* [iad_reset num_ai] resets the text box to the default prompt for
 * init_ai_diff *)
let iad_reset num_ai =
  clear_init_io_box ();
  Graphics.moveto 30 240;
  Graphics.draw_string ("Assign difficulty to " ^
                        (string_of_int num_ai) ^ " AI(s), separated");
  Graphics.moveto 30 225;
  Graphics.draw_string "by spaces, followed by 'ENTER'";
  Graphics.moveto 30 210;
  Graphics.draw_string "Difficulties are: easy, hard ";
  Graphics.moveto 30 195;
  Graphics.draw_string "> "

(* [ipn_reset ()] resets the text box to the default prompt for
 * init_player_names *)
let ipn_reset () =
  clear_init_io_box ();
  Graphics.moveto 30 240;
  Graphics.draw_string "Enter player names separated by spaces, in the order";
  Graphics.moveto 30 225;
  Graphics.draw_string "you want the turns to go, followed by 'ENTER'";
  Graphics.moveto 30 210;
  Graphics.draw_string "> "

(* [str_of_keyboard_events io_op info] is the string result of keyboard input
 * for a given io_op, and an optional [info] integer. *)
let rec str_of_keyboard_events io_op info =
  let h = match io_op with
    | `Init_num_players -> 210
    | `Init_num_humans -> 210
    | `Init_ai_diff -> 195
    | `Init_player_names -> 210 in
  let w = fst (Graphics.text_size "w") in
  let rec helper w' history =
    let curr_status = Graphics.wait_next_event [Graphics.Key_pressed] in
    let c = curr_status.Graphics.key in
    if Char.code c = 13 then
      List.fold_right (fun (c,_) acc -> (Char.escaped c)^acc) history ""
    else if Char.code c = 8 then
      (let _ = match io_op with
          | `Init_num_players -> inp_reset ()
          | `Init_num_humans -> inh_reset ()
          | `Init_ai_diff -> iad_reset info
          | `Init_player_names -> ipn_reset () in
       Graphics.moveto (30 + 2*w) h;
       let h' = remove_last_elt history in
       let w'' = List.fold_right
           (fun (c,w') acc ->
              Graphics.moveto w' h;
              Graphics.draw_string (Char.escaped c); acc + w) h' (2*w) in
       helper w'' h')
    else if Char.code c < 26 || Char.code c > 126 ||
            ((List.length history) * w > 320)then
      helper w' history
    else
      (Graphics.moveto (30+w') h;
       Graphics.draw_string (Char.escaped c);
       helper (w'+w) (history @ [(c, 30+w')]))
  in helper (2*w) []

(* [init_num_players ()] is the number of players (between 1 and 4). *)
let rec init_num_players () =
  inp_reset ();
  let s = str_of_keyboard_events `Init_num_players (-1) in
  let num_players = try int_of_string s with _ ->
    reset ();
    Graphics.moveto 30 240;
    Graphics.draw_string "Invalid entry, press any key to continue";
    let _ = Graphics.wait_next_event [Graphics.Key_pressed] in
    init_num_players () in
  if num_players > 4 || num_players < 1 then
    (reset ();
     Graphics.moveto 30 240;
     Graphics.draw_string "Invalid number, press any key to continue";
     let _ = Graphics.wait_next_event [Graphics.Key_pressed] in
     init_num_players ())
  else num_players

(* [init_num_humans total_num_players] is the number of human players. *)
let rec init_num_humans total_num_players =
  inh_reset ();
  let s = str_of_keyboard_events `Init_num_humans (-1) in
  let num_humans = try int_of_string s with _ ->
    reset ();
    Graphics.moveto 30 240;
    Graphics.draw_string "Invalid entry, press any key to continue";
    let _ = Graphics.wait_next_event [Graphics.Key_pressed] in
    init_num_humans total_num_players in
  if num_humans < 0 || num_humans > total_num_players then
    (reset ();
     Graphics.moveto 30 240;
     Graphics.draw_string "Invalid number, press any key to continue";
     let _ = Graphics.wait_next_event [Graphics.Key_pressed] in
     init_num_humans total_num_players)
  else num_humans

(* [init_ai_diff num_ai] is a list of difficulties for a number
 * [num_ai] of AIs *)
let rec init_ai_diff num_ai =
  iad_reset num_ai;
  let s = str_of_keyboard_events `Init_ai_diff num_ai in
  let ai_diff_lst = s |> Str.split (Str.regexp "[ \t]+") in
  if List.length ai_diff_lst <> num_ai then
    (reset ();
     Graphics.moveto 30 240;
     Graphics.draw_string "Invalid number of difficulties, press any key";
     Graphics.moveto 30 225;
     Graphics.draw_string "to continue";
     let _ = Graphics.wait_next_event [Graphics.Key_pressed] in
     init_ai_diff num_ai)
  else
    let rec helper lst =
      match lst with
      | [] -> []
      | h::t -> let h' = h |> String.trim |> String.lowercase_ascii in
        match h' with
        | "easy" -> Easy :: helper t
        | "hard" -> Hard :: helper t
        | _ -> (reset ();
                Graphics.moveto 30 240;
                Graphics.draw_string
                  "Invalid difficulty entry, press any key to continue";
                let _ = Graphics.wait_next_event [Graphics.Key_pressed] in
                failwith "") in
    try helper ai_diff_lst with Failure _ -> init_ai_diff num_ai

(* [init_player_names num_players] is a list of player names. *)
let rec init_player_names num_players =
  if num_players <> 0 then
    begin
      ipn_reset ();
      let player_names_str = str_of_keyboard_events `Init_player_names (-1) in
      let player_name_lst =
        player_names_str |> Str.split (Str.regexp "[ \t]+") in
      if List.length player_name_lst <> num_players then
        (reset ();
         Graphics.moveto 30 240;
         Graphics.draw_string
           "Invalid number of names, press any key to continue";
         let _ = Graphics.wait_next_event [Graphics.Key_pressed] in
         init_player_names num_players)
      else
      if List.length (List.sort_uniq Pervasives.compare player_name_lst) <>
         List.length player_name_lst then
        (reset ();
         Graphics.moveto 30 240;
         Graphics.draw_string
           "Player names must be unique, press any key to continue";
         let _ = Graphics.wait_next_event [Graphics.Key_pressed] in
         init_player_names num_players)
      else
        player_name_lst
    end
  else []

(* [init_game ()] gathers game state information from user, generates an initial
 * state [st], creates initial gui, and then calls [play_game st]. *)
let init_game () =
  reset ();
  let num_players = init_num_players () in
  let num_humans = init_num_humans num_players in
  let player_names = init_player_names num_humans in
  let ai_difficulty_lst =
    if num_humans <> num_players then
      init_ai_diff (num_players - num_humans)
    else [] in
  let st = State.init_state ({num_players = num_players;
                              num_humans = num_humans;
                              human_names = player_names;
                              ai_difficulty = ai_difficulty_lst}) in
  reset ();
  Graphics.close_graph ();
  init_gui st;
  play_game st

(* [main ()] starts the game *)
let main () =
  try
    Graphics.open_graph " 400x400";
    reset ();
    init_game ()
  with
    Graphics.Graphic_failure _ -> print_endline "\nThanks for playing!"; exit 0

let () = main ()
