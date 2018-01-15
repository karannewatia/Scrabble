open State
open Command
open Graphics
open Ai
open Trie
open Str

(* [box_config] represents a configuration for a box drawn on the gui. It holds
 * all of the necessary information to draw a cell of the scrabble grid on the
 * gui. *)
type box_config =
  {x : int;
   y : int;
   w : int;
   h : int;
   bw : int;
   b1_col : Graphics.color;
   b2_col : Graphics.color;
   b_col : Graphics.color}

(* [GuiExn] are passed to main, where the string is outputted to the user. *)
exception GuiExn of string

(* [get_chars s] splits [s] into a list of its characters. *)
let get_chars s =
  let rec split_helper s' acc =
    match s' with
    | "" -> acc
    | s'' -> split_helper (String.sub s'' 1 ((String.length s'') - 1))
               (String.get s'' 0 :: acc)
  in List.rev (split_helper s [])

(* [draw_string s x y is_h] draws string [s] in direction [is_h], with the
 * starting bottom-left coordinate at (x, y). *)
let draw_string s x y is_h =
  let rec draw_string_helper lst x y =
    match lst with
    | [] -> ()
    | h::t ->
      begin
        if is_h then
          let text_width = fst (Graphics.text_size (Char.escaped h)) in
          Graphics.moveto x y;
          Graphics.draw_char h;
          draw_string_helper t (x + text_width) y
        else
          let text_height = snd (Graphics.text_size (Char.escaped h)) in
          Graphics.moveto x y;
          Graphics.draw_char h;
          draw_string_helper t x (y - text_height)
      end
  in
  Graphics.set_color Graphics.black;
  draw_string_helper (get_chars s) x y

(* [draw_box_outline bcf col] draws the outline of a box with color [col]
 * and box_config [bcf]. *)
let draw_box_outline bcf col =
  Graphics.set_color col;
  Graphics.draw_rect bcf.x bcf.y bcf.w bcf.h

(* [draw_box bcf] draws and fills in a box with box_config [bcf]. *)
let draw_box bcf =
  let x1 = bcf.x in
  let y1 = bcf.y in
  let x2 = x1 + bcf.w in
  let y2 = y1 + bcf.h in
  let ix1 = x1 + bcf.bw in
  let ix2 = x2 - bcf.bw in
  let iy1 = y1 + bcf.bw in
  let iy2 = y2 - bcf.bw in
  let border1 g =
    Graphics.set_color g;
    Graphics.fill_poly
      [|(x1, y1); (ix1, iy1); (ix2, iy1); (ix2, iy2); (x2, y2); (x2, y1)|]
  in
  let border2 g =
    Graphics.set_color g;
    Graphics.fill_poly
      [|(x1, y1); (ix1, iy1); (ix1, iy2); (ix2, iy2); (x2, y2); (x1, y2)|]
  in
  Graphics.set_color bcf.b_col;
  Graphics.fill_rect ix1 iy1 (ix2 - ix1) (iy2 - iy1);
  border1 bcf.b1_col;
  border2 bcf.b2_col;
  draw_box_outline bcf Graphics.black

(* [earses_box bcf] erases the box with box_config [bcf]. *)
let erase_box bcf =
  Graphics.set_color bcf.b_col;
  Graphics.fill_rect (bcf.x + bcf.bw) (bcf.y + bcf.bw)
    (bcf.w - (2 * bcf.bw)) (bcf.h - (2 * bcf.bw))

(* [draw_string_in_box str bcf col] draws string [str] with color [col]
 * in the center of a box with box_config [bcf]. *)
let draw_string_in_box str bcf col =
  let (w, h) = Graphics.text_size str in
  let ty = bcf.y + (bcf.h - h) / 2 in
  Graphics.moveto (bcf.x + (bcf.w - w)/2) ty;
  Graphics.set_color col;
  Graphics.draw_string str

(* [set_rgb r g b] returns a Graphics color with red component [r], green
 * component [g], and blue component [b]. *)
let set_rgb r g b = (Graphics.rgb r g b)

let beige1 = set_rgb 215 215 195
let beige2 = set_rgb 235 235 215
let beige3 = set_rgb 255 255 240

let dark_beige1 = set_rgb 185 185 165
let dark_beige2 = set_rgb 205 205 185
let dark_beige3 = set_rgb 245 245 220

let darker_beige1 = set_rgb 135 135 115
let darker_beige2 = set_rgb 155 155 135
let darker_beige3 = set_rgb 175 175 155

let red1 = set_rgb 225 10 10
let red2 = set_rgb 250 65 65
let red3 = set_rgb 255 125 125

let green1 = set_rgb 20 140 20
let green2 = set_rgb 80 190 80
let green3 = set_rgb 120 230 120

let blue1 = set_rgb 35 85 185
let blue2 = set_rgb 65 115 215
let blue3 = set_rgb 130 170 255

let orange1 = set_rgb 240 140 15
let orange2 = set_rgb 255 170 60
let orange3 = set_rgb 255 195 110

let gray1 = set_rgb 120 120 120
let gray2 = set_rgb 160 160 160
let gray3 = set_rgb 210 210 210

let dark_gray1 = set_rgb 70 70 70
let dark_gray2 = set_rgb 110 110 110
let dark_gray3 = set_rgb 160 160 160

(* [tws_indeces] are the array indices that correspond to cells with triple
 * word multipliers. *)
let tws_indeces =
  [0; 7; 14; 105; 119; 210; 217; 224]

(* [dls_indeces] are the array indices that correspond to cells with double
 * letter multipliers. *)
let dls_indeces =
  [3; 11; 36; 38; 45; 52; 59; 92; 96; 98; 102; 108; 116;
   122; 126; 128; 132; 165; 172; 179; 186; 188; 213; 221]

(* [dws_indeces] are the array indices that correspond to cells with double
 * word multipliers. *)
let dws_indeces =
  [16; 28; 32; 42; 48; 56; 64; 70; 112; 154; 160; 168; 176; 182; 192; 196; 208]

(* [tls_indeces] are the array indices that correspond to cells with triple
 * letter multipliers. *)
let tls_indeces =
  [20; 24; 76; 80; 84; 88; 136; 140; 144; 148; 200; 204]

(* [create_grid nb_col n sep b acc] creates a grid of boxes of the same size.
 * [nb_col] corresponds to the number of columns in the grid, [n] corresponds to
 * the total number of boxes that are created in the grid, [sep] corresponds to
 * the seperation between boxes in the grid, and [b] is a box_config for the box
 * being created. A list of box_configs corresponding to each box in the grid is
 * returned. *)
let rec create_grid nb_col n sep b acc =
  if n < 0 then acc
  else
    let px = n mod nb_col in
    let py = n / nb_col in
    let nx = b.x + sep + px * (b.w + sep) in
    let ny = b.y + sep + py * (b.h + sep) in
    let b' =
      if List.mem n tws_indeces then
        {b with x = nx; y = ny; b1_col = orange1;
                b2_col = orange3; b_col = orange2}
      else if List.mem n dls_indeces then
        {b with x = nx; y = ny; b1_col = blue1; b2_col = blue3; b_col = blue2}
      else if List.mem n dws_indeces then
        {b with x = nx; y = ny; b1_col = red1; b2_col = red3; b_col = red2}
      else if List.mem n tls_indeces then
        {b with x = nx; y = ny; b1_col = green1;
                b2_col = green3; b_col = green2}
      else
        {b with x = nx; y = ny}
    in
    (create_grid nb_col (n - 1) sep b (b' :: acc))

(* [vb] corresponds to the array of box_configs that represent the board grid *)
let vb =
  let b = {x = 0; y = 0; w = 40; h = 40; bw = 2;
           b1_col = beige1; b2_col = beige3; b_col = beige2} in
  Array.of_list (create_grid 15 224 0 b [])

(* [board_to_graph_row n] returns the graph row coordinate given the board
 * row coordinate [n].
 * - example: (0, 0) in state represents the top, left cell, but when creating
 * the GUI, it represents the bottom, left cell (14, 0). *)
let board_to_graph_row n =
  14 - n

(* [coord_to_array_index c] returns the array index in the scrabble grid
 * corresponding to coordinate [c]. *)
let coord_to_array_index c =
  match c with
  | (x, y) -> 15 * (board_to_graph_row x) + y

(* [create_rack n b acc] creates boxes for each letter in the current player's
 * rack. [n] corresponds to the number of letters and [b] corresponds to the
 * box_config for the box being created. *)
let rec create_rack n b acc =
  if n < 0 then acc
  else
    let x' = b.x + b.w in
    let b' = {b with x = x'} in
    create_rack (n - 1) b' (b' :: acc)

(* [rack n] corresponds to the array of box_configs that represents the current
 * player's rack. There are [n] letters in the rack. *)
let rack n =
  let b = {x = 620; y = 350; w = 40; h = 40; bw = 2;
           b1_col = beige1; b2_col = beige3; b_col = beige2} in
  Array.of_list (create_rack (n - 1) b [])

(* [erase_rack ()] effectively erases the current player's rack from the GUI by
 * drawing a white box in the space where the rack is displayed. *)
let erase_rack () =
  Graphics.set_color Graphics.white;
  Graphics.fill_rect 660 350 300 70;
  Graphics.set_color Graphics.black

(* [rack_hidden x i len acc] returns true if the current player's rack is
 * hidden and returns false otherwise. [i] represents the current index in the
 * rack array that has length [len]. [x] represents the x coordinate of the rack
 * box with index [i]. *)
let rec rack_hidden x i len acc =
  if i >= len then acc
  else
    let pt_color = Graphics.point_color x 352 in
    rack_hidden (x + 40) (i + 1) len (acc && (pt_color = Graphics.white))

(* [draw_rack cp r_array] draws current player [cp]'s rack onto the GUI.
 * [r_array] is the array of the boxes representing the rack. *)
let draw_rack cp r_array =
  let rec draw_rack_helper r i =
    match r with
    | [] -> ()
    | (l, p)::t ->
      begin
        if l <> ' ' then
          let tile_str =
            String.capitalize_ascii (Char.escaped l) ^ " : " ^ (string_of_int p)
          in
          draw_string_in_box tile_str r_array.(i) Graphics.black;
          draw_rack_helper t (i + 1)
        else
          draw_rack_helper t (i + 1)
      end
  in
  draw_string (cp.name ^ "'s rack:") 660 400 true;
  Array.iter draw_box r_array;
  draw_rack_helper (List.rev cp.rack) 0

(* [erase_toggle_button ()] erases the toggle button. *)
let erase_toggle_button () =
  Graphics.set_color Graphics.white;
  Graphics.fill_rect 632 120 60 60;
  Graphics.set_color Graphics.black

(* [toggle_rack cp] draws the current player [cp]'s rack on the GUI if it is
 * currently hidden and hides the rack if it is currently displayed. *)
let toggle_rack cp =
  let len_rack = List.length cp.rack in
  let r_array = rack len_rack in
  let is_rack_hidden = rack_hidden 662 0 len_rack true in
  let b_toggle_rack = {x = 632; y = 120; w = 60; h = 60; bw = 2;
                       b1_col = gray1; b2_col = gray3;
                       b_col = gray2} in
  erase_toggle_button ();
  draw_box b_toggle_rack;
  if is_rack_hidden && len_rack > 0 then
    begin
      draw_string_in_box "Hide rack" b_toggle_rack Graphics.black;
      draw_rack cp r_array;
    end
  else
    begin
      draw_string_in_box "Show rack" b_toggle_rack Graphics.black;
      erase_rack ()
    end

(* [draw_buttons is_rack_hidden] draws all of the buttons used to perform
* different actions in the game. [is_rack_hidden] is a boolean that states
* whether or not the rack is displayed on the GUI. *)
let draw_buttons is_rack_hidden =
  let b_pass = {x = 632; y = 30; w = 60; h = 60; bw = 2;
                b1_col = gray1; b2_col = gray3; b_col = gray2} in
  draw_box b_pass;
  draw_string_in_box "Pass Turn" b_pass Graphics.black;
  let b_help = {x = 724; y = 30; w = 60; h = 60; bw = 2;
                b1_col = gray1; b2_col = gray3; b_col = gray2} in
  draw_box b_help;
  draw_string_in_box "Help" b_help Graphics.black;
  let b_hint = {x = 816; y = 30; w = 60; h = 60; bw = 2;
                b1_col = gray1; b2_col = gray3; b_col = gray2} in
  draw_box b_hint;
  draw_string_in_box "Hint" b_hint Graphics.black;
  let b_quit = {x = 908; y = 30; w = 60; h = 60; bw = 2;
                b1_col = gray1; b2_col = gray3; b_col = gray2} in
  draw_box b_quit;
  draw_string "Quit" 927 63 true;
  draw_string "Game" 927 45 true;
  let b_toggle_rack = {x = 632; y = 120; w = 60; h = 60; bw = 2;
                b1_col = gray1; b2_col = gray3; b_col = gray2} in
  draw_box b_toggle_rack;
  if is_rack_hidden then
    draw_string_in_box "Show Rack" b_toggle_rack Graphics.black
  else
    draw_string_in_box "Hide Rack" b_toggle_rack Graphics.black;
  let b_add_word = {x = 724; y = 120; w = 60; h = 60; bw = 2;
                b1_col = gray1; b2_col = gray3; b_col = gray2} in
  draw_box b_add_word;
  draw_string "Add To" 737 153 true;
  draw_string "Dict" 743 135 true;
  let b_swap = {x = 816; y = 120; w = 60; h = 60; bw = 2;
                b1_col = gray1; b2_col = gray3; b_col = gray2} in
  draw_box b_swap;
  draw_string "Swap" 835 153 true;
  draw_string "Tiles" 832 135 true;
  let b_place = {x = 908; y = 120; w = 60; h = 60; bw = 2;
                b1_col = gray1; b2_col = gray3; b_col = gray2} in
  draw_box b_place;
  draw_string "Place" 924 153 true;
  draw_string "Word" 927 135 true

(* [dark_gray_place ()] changes the color of the place button to dark gray. *)
let dark_gray_place () =
  let b_place = {x = 908; y = 120; w = 60; h = 60; bw = 2;
                 b1_col = dark_gray1; b2_col = dark_gray3; b_col = dark_gray2} in
  draw_box b_place;
  draw_string "Place" 924 153 true;
  draw_string "Word" 927 135 true

(* [update_vb b] takes a flattened board [b] and replaces any colored cells with
 * letters in them with standard beige colors in [vb]. This represents a
 * multiplier being used up. *)
let rec update_vb b =
  match b with
  | [] -> ()
  | cell::t ->
   let array_idx = coord_to_array_index cell.cell_coord in
   let array_cell = Array.get vb array_idx in
   let array_cell' =
     if fst cell.letter <> ' ' then
       {array_cell with
        b1_col = dark_beige1;b2_col = dark_beige3;b_col = dark_beige2}
     else if List.mem (coord_to_array_index cell.cell_coord) tws_indeces then
       {array_cell with b1_col = orange1;b2_col = orange3;b_col = orange2}
     else if List.mem (coord_to_array_index cell.cell_coord) dls_indeces then
       {array_cell with b1_col = blue1;b2_col = blue3;b_col = blue2}
     else if List.mem (coord_to_array_index cell.cell_coord) dws_indeces then
       {array_cell with b1_col = red1;b2_col = red3;b_col = red2}
     else if List.mem (coord_to_array_index cell.cell_coord) tls_indeces then
       {array_cell with b1_col = green1;b2_col = green3;b_col = green2}
     else
       {array_cell with b1_col = beige1;b2_col = beige3;b_col = beige2}
   in
   Array.set vb array_idx array_cell';
   update_vb t

(* [update_board b] updates the GUI with changes in the flattened board [b]
 * whenever a move is made. *)
let update_board b =
  let rec update_board_helper b' =
    match b' with
    | [] -> ()
    | cell::t ->
      if fst cell.letter = ' ' && cell.cell_coord = (7,7) then
        draw_string_in_box "START" vb.(112) Graphics.black;
      if fst cell.letter <> ' ' then
        (draw_string_in_box
           (String.capitalize_ascii (Char.escaped (fst cell.letter)))
           vb.(coord_to_array_index (cell.cell_coord)) Graphics.black;
         update_board_helper t)
      else
        let str_mult =
          if cell.cell_coord <> (7,7) then
            match cell.letter_multiplier, cell.word_multiplier with
            | 2, 1 -> "DL"
            | 3, 1 -> "TL"
            | 1, 2 -> "DW"
            | 1, 3 -> "TW"
            | 1, 1 -> ""
            | _ -> failwith "impossible"
          else ""
        in
        draw_string_in_box (String.capitalize_ascii str_mult)
          vb.(coord_to_array_index (cell.cell_coord)) Graphics.black;
        update_board_helper t
  in
  Array.iter draw_box vb;
  update_board_helper b

(* [draw_logo ()] draws the Scrabble logo at the top of the GUI at the start of
 * each new game. *)
let draw_logo () =
  let bar_height = snd (Graphics.text_size "|") in
  draw_string "                               _       _       _         " 625 575 true;
  draw_string "  ___    ___    _ __    __ _  | |__   | |__   | |   ___  " 625 (575 - bar_height) true;
  draw_string " / __|  / __|  | '__| / _` | |  _ \\ |  _ \\ | |  / _ \\ " 625 (575 - 2 * bar_height) true;
  draw_string " \\__\\| (__   | |    | (_| | | |_) | | |_) | | | |  __/ " 625 (575 - 3 * bar_height) true;
  draw_string " |___/  \\___| |_|    \\__,_| |_.__/  |_.__/  |_|  \\___|" 625 (575 - 4 * bar_height) true;
  draw_string " ________________________________________________________" 625 (575 - 5 * bar_height) true

(* [draw_io_box ()] draws the input / output box. *)
let draw_io_box () =
  draw_rect 620 210 360 100

(* [compare_players p1 p2] compares the order numbers of players [p1] and [p2]. *)
let compare_players p1 p2 =
  if p1.order_num < p2.order_num then 1 else -1

(* [update_scores ps] takes a list of players [ps] and draws the scores of each
 * player on the GUI after each turn. *)
let update_scores ps =
  set_color white;
  fill_rect 620 400 200 100;
  set_color black;
  let sorted_ps = List.sort compare_players ps in
  let h = snd (Graphics.text_size "|") in
  let w = fst (Graphics.text_size "w") in
  draw_string "Scores:" 625 (575 - 7 * h) true;
  let rec helper ps' =
    match ps' with
    | [] -> ()
    | p::t -> draw_string (p.name ^ ": " ^ (string_of_int p.score))
                632 (575 - (8 + (List.length t)) * h) true;
              helper t
  in helper sorted_ps;
  let max_name_len = List.fold_left
      (fun acc p -> if String.length p.name > acc then
                      String.length p.name
                    else acc) 0 ps in
  let box_width = w * max_name_len + 50 in
  let box_height = h * (List.length ps + 1) + 5 in
  Graphics.draw_rect 620 (575 - box_height - 6 * h) box_width box_height

(* [erase_turn ()] clears the turn indicator in gui window *)
let erase_turn () =
  Graphics.set_color Graphics.white;
  Graphics.fill_rect 770 470 200 25;
  Graphics.set_color Graphics.black

(* [update_gui st] takes the current state [st] and updates the GUI after each
 * move is made. *)
let update_gui cmd st =
  match cmd with
  | `Place ->
    let len_rack = List.length st.current_player.rack in
    let r_array = rack len_rack in
    let is_rack_hidden = rack_hidden 662 0 len_rack true in
    update_vb (List.flatten st.board);
    update_board (List.flatten st.board);
    update_scores st.players;
    draw_buttons is_rack_hidden;
    erase_turn ();
    draw_string (st.current_player.name ^ "'s turn.") 770 470 true;
    if is_rack_hidden then ()
    else
      draw_rack st.current_player r_array
  | `Help ->
    set_color white;
    fill_rect 601 0 399 600;
    set_color black;
    let len_rack = List.length st.current_player.rack in
    let r_array = rack len_rack in
    let is_rack_hidden = rack_hidden 662 0 len_rack true in
    draw_logo ();
    update_scores st.players;
    draw_buttons is_rack_hidden;
    draw_io_box ();
    draw_string (st.current_player.name ^ "'s turn.") 770 470 true;
    if is_rack_hidden then ()
    else
      draw_rack st.current_player r_array
  | `Swap ->
    erase_turn ();
    let len_rack = List.length st.current_player.rack in
    let r_array = rack len_rack in
    let is_rack_hidden = rack_hidden 662 0 len_rack true in
    draw_string (st.current_player.name ^ "'s turn.") 770 470 true;
    draw_buttons true;
    if is_rack_hidden then ()
    else
      draw_rack st.current_player r_array
  | `Pass ->
    erase_turn ();
    draw_string (st.current_player.name ^ "'s turn.") 770 470 true

(* [mem (x,y) (x0,y0,w,h)] is true if (x,y) is within the rectangle specified
 * by (x0,y0,w,h) and false otherwise *)
let mem (x, y) (x0, y0, w, h) =
  (x >= x0) && (x < x0 + w) && (y >= y0) && (y < y0 + h)

(* coordinates of rectangle representing the pass button *)
let pass_btn = (632, 30, 60, 60)

(* coordinates of rectangle representing the help button *)
let help_btn = (724, 30, 60, 60)

(* coordinates of rectangle representing the hint button *)
let hint_btn = (816, 30, 60, 60)

(* coordinates of rectangle representing the quit button *)
let quit_btn = (908, 30, 60, 60)

(* coordinates of rectangle representing the show rack button *)
let toggle_rack_btn = (632, 120, 60, 60)

(* coordinates of rectangle representing the hide rack button *)
let add_btn = (724, 120, 60, 60)

(* coordinates of rectangle representing the swap button *)
let swap_btn = (816, 120, 60, 60)

(* coordinates of rectangle representing the place button *)
let place_btn = (908, 120, 60, 60)

(* [get_rack_coords rack_len] is the list of lower-left corners of boxes
 * corresponding to the characters in the players rack. *)
let rec get_rack_coords rack_len =
  match rack_len with
  | 0 -> []
  | _ -> (620 + rack_len * 40, 350) :: get_rack_coords (rack_len - 1)

let rec board_coords row_index len =
  match len with
  | 15 -> []
  | _ ->
    (row_index * 40, (40 * len)) :: board_coords row_index (len + 1)

let rec all_cells index =
  match index with
  | 15 -> []
  | _ ->
    (board_coords index 0) @ all_cells (index + 1)

(* [get_idx_from_coord x] is the rack index corresponding to a given x coord in
 * the gui. *)
let get_idx_from_coord x =
  (x - 660) / 40

(* [get_cell_from_pixel (x, y)] converts the gui board cooridnate for a cell
 * to the coordinate required for making a move in state. *)
let get_cell_from_pixel (x, y) =
  (14 - (y / 40), x / 40)

(* [sort_horizontal ((_,y1),_) ((_,y2),_)] is used for sorting pairs of cells
 * and corresponding letters when the tiles are placed in the horizontal
 * direction.
 *)
let sort_horizontal ((_,y1),_) ((_,y2),_) =
  if y1 < y2 then -1
  else if y1 > y2 then 1
  else 0

(* [sort_horizontal ((_,y1),_) ((_,y2),_)] is used for sorting pairs of cells
 * and corresponding letters when the tiles are placed in the vertical
 * direction.
 *)
let sort_vertical ((x1,_),_) ((x2,_),_) =
  if x1 < x2 then -1
  else if x1 > x2 then 1
  else 0

(* [get_input_one_tile lst st] converts the coordinate-letter pair in lst
 * to a triple which is used for conversion into a move type in state [st].
 * requires: [lst] is of length 1
 *)
let get_input_one_tile lst st =
  let letter = snd (List.nth lst 0) |> Char.escaped in
  let coord = fst (List.nth lst 0) in
  let cell = get_cell_from_coordinate coord st in
  let all_words = get_all_adj_words (cell) st in
  match (left_cell cell, up_cell cell) with
  | Some c', None ->
    let cell' = get_cell_from_coordinate c' st in
    if cell_is_empty cell' then
      if List.nth all_words 1 <> "" then
        coord, letter ^ (List.nth all_words 1), true
      else coord, letter ^ (List.nth all_words 3), false
    else
      begin
      match get_adjacent_word c' st true [] with
      | None ->
        let letter' = fst cell'.letter |> Char.escaped in
        c', letter' ^ letter ^ (List.nth all_words 1), true
      | Some (str,_,_) ->
        let new_cell = fst c', (snd c') + 1 - String.length str in
        new_cell, str ^ letter ^ (List.nth all_words 1), true
    end
  | None, Some c' ->
    let cell' = get_cell_from_coordinate c' st in
    if cell_is_empty cell' then
      if List.nth all_words 1 <> "" then
        coord, letter ^ (List.nth all_words 1), true
      else coord, letter ^ (List.nth all_words 3), false
    else
      begin
        match get_adjacent_word c' st false [] with
        | None ->
          let letter' = fst cell'.letter |> Char.escaped in
          c', letter' ^ letter ^ (List.nth all_words 3), false
        | Some (str,_,_) ->
          let new_cell = fst c' + 1 - String.length str, snd c' in
          new_cell, str ^ letter ^ (List.nth all_words 3), false
      end
  | Some c', Some c'' ->
    let cell' = get_cell_from_coordinate c' st in
    let cell'' = get_cell_from_coordinate c'' st in
    if cell_is_empty cell' then
      if List.nth all_words 1 <> "" then
        coord, letter ^ (List.nth all_words 1), true
      else
        if cell_is_empty cell'' then
          coord, letter ^ (List.nth all_words 3), false
        else
          begin
            match get_adjacent_word c'' st false [] with
            | None ->
              let letter' = fst cell''.letter |> Char.escaped in
              c'', letter' ^ letter ^ (List.nth all_words 3), false
            | Some (str,_,_) ->
              let new_cell = fst c'' + 1 - String.length str, (snd c'') in
              new_cell, str ^ letter ^ (List.nth all_words 3), false
          end
    else
      begin
        match get_adjacent_word c' st true [] with
        | None ->
          let letter' = fst cell'.letter |> Char.escaped in
          c', letter' ^ letter ^ (List.nth all_words 1), true
        | Some (str,_,_) ->
          let new_cell = fst c', (snd c') + 1 - String.length str in
          new_cell, str ^ letter ^ (List.nth all_words 1), true
      end
  | None, None ->
    if (List.nth all_words 1) <> "" then
      coord, letter ^ (List.nth all_words 1), true
    else coord, letter ^ (List.nth all_words 3), false

(* [get_input_vertical lst st] converts the cell-letter pairs in lst
 * to a triple which is used for conversion into a move type in state [st,
 * where the move is in the vertical direction.
 * requires: [lst] is of length more than 1
 * raises:
 * - [GuiExn "cannot place new tiles on top of existing tiles"] if any of the
 * coordinate-letter pairs in [lst] has a coordinate that is the same
 * as any of the already existing tile coordinates in [st]
 * -  [GuiExn "tiles were not placed on same row/column"] if all the coordinates
 * in [lst] do not have the same y-coordinate.
 *)
let get_input_vertical lst st =
  let first_click = snd (fst (List.nth lst 0)) in
  let enforce_same_column =
    List.fold_left
      (fun acc x ->
         (acc && (first_click = snd (fst x)))
      ) true (List.tl lst) in
  if enforce_same_column then
    let update_cells = List.sort sort_vertical lst in
    let leftmost_input = List.nth update_cells 0 in
    let leftmost_cell = get_cell_from_coordinate (fst leftmost_input) st in
    let first_cell =
      match up_cell (leftmost_cell) with
      | None -> fst leftmost_input
      | Some c' ->
        let cell' = get_cell_from_coordinate c' st in
        match get_adjacent_word c' st false [] with
        | None ->
          if not ( cell_is_empty cell') then
            fst (fst leftmost_input) - 1, snd (fst leftmost_input)
          else fst leftmost_input
        | Some (str,_,_) ->
          fst (fst leftmost_input) - String.length str,
          snd (fst leftmost_input)  in
    let rightmost_input =
      List.nth update_cells ((List.length update_cells) - 1) in
    let rightmost_cell =
      get_cell_from_coordinate (fst rightmost_input) st in
    let last_cell =
      match down_cell (rightmost_cell) with
      | None -> fst rightmost_input
      | Some c' ->
        let cell' = get_cell_from_coordinate c' st in
        match get_adjacent_word c' st false [] with
        | None ->
          if not ( cell_is_empty cell') then
            fst (fst rightmost_input) + 1, snd (fst rightmost_input)
          else fst rightmost_input
        | Some (str,_,_) ->
          fst (fst rightmost_input) + String.length str,
          snd (fst rightmost_input) in
    let col = get_column (fst leftmost_input) st
              |> List.filter (fun c' ->
                  fst (c'.cell_coord) >= fst (first_cell) &&
                  fst (c'.cell_coord) <= fst (last_cell) ) in
    let added_str =
      List.fold_left
        (fun acc x ->
           let coord = x.cell_coord in
           if not (cell_is_empty x) then
             match List.assoc_opt (coord) update_cells with
             | None -> (fst (x.letter) |> Char.escaped) ^ acc
             | Some _ ->
               raise (GuiExn "cannot place new tiles on top of existing tiles")
           else match List.assoc_opt (coord) update_cells with
             | None -> " "
             | Some letter -> (Char.escaped letter) ^ acc
        ) "" col |> reverse_str in
    let cell = get_cell_from_coordinate (fst leftmost_input) st in
    match up_cell cell with
    | None -> (fst leftmost_input), added_str, false
    | Some c' ->
      let cell' = get_cell_from_coordinate c' st in
      if cell_is_empty cell' then (fst leftmost_input), added_str, false
      else
        match get_adjacent_word c' st false [] with
        | None ->
          c', added_str, false
        | Some (str,_,_) ->
          let new_cell = fst c' + 1 - String.length str , (snd c') in
          new_cell, added_str, false
  else raise (GuiExn "tiles were not placed on same row/column")

(* [get_input_horizontal lst st] converts the cell-letter pairs in lst
 * to a triple which is used for conversion into a move type in state [st,
 * where the move is in the horizontal direction.
 * requires: [lst] is of length more than 1
 * raises:
 * - [GuiExn "cannot place new tiles on top of existing tiles"] if any of the
 * coordinate-letter pairs in [lst] has a coordinate that is the same
 * as any of the already existing tile coordinates in [st]
 * -  [GuiExn "tiles were not placed on same row/column"] if all the coordinates
 * in [lst] do not have the same x-coordinate.
 *)
let get_input_horizontal lst st =
  let first_click = fst (fst (List.nth lst 0)) in
  let enforce_same_row =
    List.fold_left
      (fun acc x ->
         (acc && (first_click = fst (fst x)))
      ) true (List.tl lst) in
  if enforce_same_row then
    let update_cells = List.sort sort_horizontal lst in
    let leftmost_input = List.nth update_cells 0 in
    let leftmost_cell = get_cell_from_coordinate (fst leftmost_input) st in
    let first_cell =
      match left_cell (leftmost_cell) with
      | None -> fst leftmost_input
      | Some c' ->
        let cell' = get_cell_from_coordinate c' st in
        match get_adjacent_word c' st true [] with
        | None ->
          if not ( cell_is_empty cell') then
            fst (fst leftmost_input), snd (fst leftmost_input) - 1
          else fst leftmost_input
        | Some (str,_,_) ->
          fst (fst leftmost_input),
          snd (fst leftmost_input) - String.length str in
    let rightmost_input =
      List.nth update_cells ((List.length update_cells) - 1) in
    let rightmost_cell = get_cell_from_coordinate (fst rightmost_input) st in
    let last_cell =
      match right_cell (rightmost_cell) with
      | None -> fst rightmost_input
      | Some c' ->
        let cell' = get_cell_from_coordinate c' st in
        match get_adjacent_word c' st true [] with
        | None ->
          if not ( cell_is_empty cell') then
            fst (fst rightmost_input), snd (fst rightmost_input) + 1
          else fst rightmost_input
        | Some (str,_,_) ->
          fst (fst rightmost_input),
          snd (fst rightmost_input) + String.length str in
    let row = get_row (fst leftmost_input) st
              |> List.filter (fun c' ->
                  snd (c'.cell_coord) >= snd (first_cell) &&
                  snd (c'.cell_coord) <= snd (last_cell) ) in
    let added_str =
      List.fold_left
        (fun acc x ->
           let coord = x.cell_coord in
           if not (cell_is_empty x) then
             match List.assoc_opt (coord) update_cells with
             | None -> (fst (x.letter) |> Char.escaped) ^ acc
             | Some _ ->
               raise (GuiExn "cannot place new tiles on top of existing tiles")
           else match List.assoc_opt (coord) update_cells with
             | None -> " "
             | Some letter -> (Char.escaped letter) ^ acc
        ) "" row  |> reverse_str in
    let cell = get_cell_from_coordinate (fst leftmost_input) st in
    match left_cell cell with
    | None -> (fst leftmost_input), added_str, true
    | Some c' ->
      let cell' = get_cell_from_coordinate c' st in
      if cell_is_empty cell' then (fst leftmost_input), added_str, true
      else
        match get_adjacent_word c' st true [] with
        | None -> c', added_str, true
        | Some (str,_,_) ->
          let new_cell = fst c', (snd c') + 1 - String.length str in
          new_cell, added_str, true
  else raise (GuiExn "tiles were not placed on same row/column")

(* [get_input lst st] converts the cell-letter pair(s) in lst
 * to a triple which is used for conversion into a move type in state [st].
 * requires: [lst] is of length more than 0
 * raises:
 * - [GuiExn "cannot place new tiles on top of existing tiles"] if any of the
 * coordinate-letter pairs in [lst] has a coordinate that is the same
 * as any of the already existing tile coordinates in [st]
 * -  [GuiExn "tiles were not placed on same row/column"] if all the coordinates
 * in [lst] do not have the same y-coordinate.
 * - [GuiExn "no letters were placed"] if length of [lst] is 0.
 *)
let get_input lst st =
  if List.length lst = 0 then raise (GuiExn "no letters were placed")
  else if List.length lst > 1 then
    if fst (fst (List.nth lst 0)) <> fst (fst (List.nth lst 1)) (* vertical*)
    then get_input_vertical lst st
    else (* horizontal*)
    get_input_horizontal lst st
  else (* only 1 tile added *)
    get_input_one_tile lst st

(* [convert_to_move lst st] converts the cell-letter pair(s) in lst
 * a 'move' type in state [st].
 * requires: [lst] is of length more than 0
 * raises:
 * - [GuiExn "cannot place new tiles on top of existing tiles"] if any of the
 * coordinate-letter pairs in [lst] has a coordinate that is the same
 * as any of the already existing tile coordinates in [st]
 * -  [GuiExn "tiles were not placed on same row/column"] if all the coordinates
 * in [lst] do not have the same y-coordinate.
 * - [GuiExn "no letters were placed"] if length of [lst] is 0.
*)
let convert_to_move lst st =
  try
    let mv = get_input lst st in
    {
      word = snd_triple mv |> explode; mv_coord = fst_triple mv;
      is_horizontal = trd_triple mv
    }
  with Failure f -> raise (GuiExn f)

(* [refresh_cell c b] is an updated board with a specified cell coordinate's
 * data updated. *)
let rec refresh_cell c b =
  match b with
  | [] -> []
  | c'::t -> if c'.cell_coord = c.cell_coord then
      c :: refresh_cell c t else c' :: refresh_cell c t

(* [remove_from_rack l r] is the updated rack with letter [l] removed *)
let rec remove_from_rack l r =
  match r with
  | [] -> []
  | (l',p)::t -> if l' = l then t else (l',p) :: remove_from_rack l t


(* [remove_last_elt lst] removes the last element of [lst] *)
let rec remove_last_elt lst =
  match lst with
  | [] -> []
  | h::[] -> []
  | h::t -> h :: remove_last_elt t

(* [erase_io_box ()] erases the io box *)
let erase_io_box () =
  Graphics.set_color Graphics.white;
  Graphics.fill_rect 621 211 358 98;
  Graphics.set_color Graphics.black

(* [add_word_reset st] redraws the window to deal with backspaces during
 * keyboard entry in str_of_keyboard_events *)
let addword_reset st =
  erase_io_box ();
  moveto 625 290;
  Graphics.draw_string
    "Type the word you wish to add to the dictionary, followed";
  moveto 625 275;
  Graphics.draw_string "by 'ENTER':";
  moveto 625 260;
  Graphics.draw_string "> "

(* [blank_tile_reset st] redraws the window to deal with the case a blank tile
 * was clicked and the user was prompted to type the preferred letter for the
 * blank during keyboard entry in str_of_keyboard_events *)
let blank_tile_reset st =
  erase_io_box ();
  moveto 625 290;
  Graphics.draw_string "Type the letter you wish to be played in place of the";
  moveto 625 275;
  Graphics.draw_string "blank tile, followed by 'ENTER':";
  moveto 625 260;
  Graphics.draw_string "> "

(* [str_of_keyboard_events st io_op] is the string result of keyboard input for
 * a given io_op. *)
let rec str_of_keyboard_events st io_op =
  let w = fst (text_size "w") in
  let rec helper w' history =
    let curr_status = wait_next_event [Key_pressed] in
    let c = curr_status.key in
    if Char.code c = 13 then
      List.fold_right (fun (c,_) acc -> (Char.escaped c)^acc) history ""
    else if Char.code c = 8 then
      (let _ = match io_op with
       | `AddWord -> addword_reset st
       | `Blank -> blank_tile_reset st in
       moveto (625+2*w) 260;
       let h' = remove_last_elt history in
       let w'' = List.fold_right
           (fun (c,w') acc ->
              moveto w' 260;
              Graphics.draw_string (Char.escaped c); acc + w) h' (2*w) in
       helper w'' h')
    else if Char.code c < 26 || Char.code c > 126 ||
            ((List.length history) * w > 320)then
      helper w' history
    else
      (moveto (625+w') 260;
       Graphics.draw_string (Char.escaped c);
       helper (w'+w) (history @ [(c ,625+w')]))
  in helper (2*w) []

(* [addword_helper st] is a string corresponding to a word that a user wishes to
 * add to the dictionary. The string is received by keyboard input *)
let addword_helper st =
  addword_reset st;
  str_of_keyboard_events st `AddWord

(* [blank_helper st] is the letter that the user types in to represent with
 * their blank tile.
 * raises: [GuiExn "invalid blank selection"] if the letter is not a single
 * uppercase or lowercase letter in the English alphabet. *)
let blank_helper st =
  blank_tile_reset st;
  let regex = Str.regexp "[A-Za-z]" in
  let new_char = str_of_keyboard_events st `Blank in
  if ((String.length new_char = 1) && Str.string_match regex new_char 0)
  then
    (blank_tile_reset st;
     moveto 625 245;
     Graphics.draw_string "Click a board cell to place the blank tile";
     String.get new_char 0)
  else raise (GuiExn "invalid blank selection")

(* [update_tile_color r_array position colors] sets the element of [r_array]
 * located at [position] to the three colors defined in [colors].
 * requires: [position] is a valid index of [r_array]. *)
let update_tile_color r_array position colors =
  let (col1, col2, col) = colors in
  let new_rack_tile = {r_array.(position) with
    b1_col = col1; b2_col = col2; b_col = col}
  in
  Array.set r_array position new_rack_tile

(* [color_tile_get_letter st rack_index colors] sets [st]'s current player's
 * rack at [rack_index] to the colors defined in [colors], then returns the
 * letter at that tile.
 * requires: [rack_index] is a valid index of [st.current_player.rack]. *)
let color_tile_get_letter st rack_index colors =
  let p_r = st.current_player.rack in
  let rack_len = List.length p_r in
  let rack_array = rack rack_len in
    update_tile_color rack_array (rack_len - rack_index - 1) colors ;
    draw_rack st.current_player rack_array ;
  let letter = fst (List.nth p_r rack_index) in
    if letter = '*' then ((blank_helper st), true)
    else (letter, false)

(* [get_rack_index s rack_coords] is the index of the rack clicked, given that
 * [s] is a Button_down event. Returns -1 if the click was not in the rack. *)
let get_rack_index s rack_coords =
  List.fold_left (fun acc (r_x, r_y) ->
    if mem (s.mouse_x, s.mouse_y) (r_x, r_y, 40, 40) then
      (get_idx_from_coord r_x)
    else acc
  ) (-1) rack_coords

(* [swap_helper cp] is a character list corresponding to rack boxes in the gui
 * that are clicked on prior to clicking the 'swap' button to finalize the swap
 * command.
 * requires: 'swap' button was clicked prior to initial function call
 * raises: [GuiExn "no tiles are being swapped"] if the user presses the Swap
 *          button again without selecting any tiles to swap. *)
let swap_helper cp =
  let my_rack = cp.rack in
  let rack_len = List.length my_rack in
  let r_array = rack rack_len in
  (* function that makes swap_helper tail recursive *)
  let rec swap_helper' swap_list colored_rack =
    let s = wait_next_event [Button_down] in
    (* end recursion when user presses the Swap button again *)
    if mem (s.mouse_x, s.mouse_y) swap_btn then
      if swap_list = [] then raise (GuiExn "no tiles are being swapped")
      else List.map (fun x -> fst (List.nth my_rack x)) swap_list
    else
      let rack_coords = get_rack_coords rack_len in
      let rack_index = get_rack_index s rack_coords in
      if rack_index = -1 then swap_helper' swap_list colored_rack
      else
        let position = rack_len - rack_index - 1 in
        if (List.mem rack_index swap_list) then
          let () = update_tile_color colored_rack position
            (beige1, beige3, beige2)
          in
          draw_rack cp colored_rack ;
          swap_helper' (State.remove rack_index swap_list) colored_rack
        else
          let () = update_tile_color colored_rack position
            (dark_beige1, dark_beige3, dark_beige2)
          in
          draw_rack cp colored_rack ;
          swap_helper' (rack_index :: swap_list) colored_rack
  in
  swap_helper' [] r_array

(* [remove_letter_at_coord board vb_coord] takes the cell located at [vb_coord]
 * on [board], and replaces its letter with (' ', -1).
 * requires: [vb_coord] is a valid coordinate of [board]. *)
let remove_letter_at_coord board vb_coord =
  List.map (
    fun cl ->
      List.map (
        fun c ->
          let c_coord = coord_to_array_index c.cell_coord in
          if c_coord = vb_coord then {c with letter = (' ', -1)}
          else c
      ) cl
  ) board

(* [shade_np_tiles pc] returns unit. It shades all of the coordinates
 * in [pc] dark_beige. *)
let shade_np_tiles pc =
  let shade_placed_tile (coord, (letter, is_blank)) =
      update_tile_color vb coord (darker_beige1,darker_beige3,darker_beige2);
    let capital_string = (String.capitalize_ascii (Char.escaped letter)) in
    draw_box vb.(coord);
    draw_string_in_box capital_string vb.(coord) Graphics.black;
  in
  (List.iter shade_placed_tile pc)

(* [return_to_rack st vb_coord np_tiles acc] is [st', pc', acc'] if
 * removing the newly-placed tile at [vb_coord] results in a new state [st'],
 * adjusted np_tiles [pc'] and an adjusted accumulator [acc']. If there
 * is no newly_placed tile at [vb_coord], then [st, pc, acc] is returned.
 * This function also visually updates the GUI to reflect these changes. *)
let return_to_rack st vb_coord np_tiles acc =
  (* is_placed represents whether there is a newly-placed tile at vb_coord *)
  let is_placed = (np_tiles |> List.split |> fst |> List.mem vb_coord) in
  if (not is_placed) then
    st, np_tiles, acc
  else
    (* guaranteed to be found, because [vb_coord] is mem of [coords] *)
    let coord_data = List.find (fun (x, _) -> x = vb_coord) np_tiles in
    let (_, (letter, is_blank)) = coord_data in
    let letter_to_put_in_rack =
      if is_blank then ('*', 0)
      else (letter, (State.get_points letter))
    in
    (* update current_player *)
    let new_rack = (letter_to_put_in_rack :: st.current_player.rack) in
    let new_current_player = {st.current_player with rack = new_rack} in
    (* visually update the rack *)
    erase_rack ();
    new_rack |> List.length |> rack |> draw_rack new_current_player;
    (* update the state board *)
    let b' = remove_letter_at_coord st.board vb_coord in
    (* visually update the board *)
    update_vb (List.flatten b');
    update_board (List.flatten b');
    (* remove the tile from np_tiles and acc *)
    let st' = {st with current_player = new_current_player; board = b'} in
    let pc' = (remove_from_rack vb_coord np_tiles) in
    let acc' = List.filter
      (fun ((x,_),_) -> (coord_to_array_index x) <> vb_coord) acc
    in
    (* final return value of return_to_rack *)
    (st', pc', acc')

(* [get_board_coord s] is the coordinates of the board that were clicked given
 * a Button_down event [s]. If the click was not inside the board,
 * then return (-1, -1) *)
let get_board_coord s =
  List.fold_left
    (fun fold_acc (r_x, r_y) ->
      if mem (s.mouse_x, s.mouse_y) (r_x, r_y, 40, 40) then
        get_cell_from_pixel (r_x, r_y)
      else fold_acc
    ) (-1, -1) (all_cells 0)

(* [place_helper st] is a list of ((cell, coord), st) entries corresponding to
 * new letters placed onto the board, forming a potentially-valid place command
 *)
let rec place_helper st np_tiles acc =
  let s = wait_next_event [Button_down] in
  (* end recursion when the Place button is pressed again *)
  if mem (s.mouse_x, s.mouse_y) place_btn then acc
  else
    let cell_coord = get_board_coord s in
    (* if first click is located on the board, try to remove a tile *)
    if (cell_coord <> (-1, -1)) then
      let vb_coord = (coord_to_array_index cell_coord) in
      let (st', np_tiles', acc') =
        (return_to_rack st vb_coord np_tiles acc)
      in
      (* shade newly-placed tiles and recursively call place_helper *)
      shade_np_tiles np_tiles';
      (place_helper st' np_tiles' acc')
    else
      let rack_len = List.length st.current_player.rack in
      let rack_coords = get_rack_coords rack_len in
      let rack_index = get_rack_index s rack_coords in
      (* if first click is neither in rack nor board, get a new first click *)
      if rack_index = -1 then (place_helper st np_tiles acc)
      else
        (* clicked on rack. darken tile, and call second_click_helper *)
        let colors = (dark_beige1, dark_beige3, dark_beige2) in
        let letter, is_blank = (color_tile_get_letter st rack_index colors) in
        (second_click_helper st (letter, is_blank) rack_index np_tiles acc)

(* [second_click_helper st (letter, is_blank) rack_index np_tiles acc] receives
 * the user's second click, and calls additional functions based on the
 * location of the click. *)
and second_click_helper st (letter, is_blank) rack_index np_tiles acc =
  let s' = wait_next_event [Button_down] in
  (* if click is on the board *)
  if mem (s'.mouse_x, s'.mouse_y) (0,0,600,600) then
    second_click_on_board st s' (letter, is_blank) rack_index np_tiles acc
  else
    let rack_len = List.length st.current_player.rack in
    let rack_coords = get_rack_coords rack_len in
    (* check to see if second click is inside the rack *)
    let new_rack_index = (get_rack_index s' rack_coords) in
    if (new_rack_index <> -1) then
      (second_click_on_rack st rack_index new_rack_index np_tiles acc)
    else if mem (s'.mouse_x, s'.mouse_y) place_btn then
      acc
    else
      (second_click_helper st (letter, is_blank) rack_index np_tiles acc)

(* [second_click_on_board st s' (letter, is_blank) rack_index np_tiles acc]
 * accounts for the case when the second place click occurs on the board. If
 * there is an oldly placed tile, nothing happens. Else, the tile is placed at
 * that location. Additionally, if there is a newly placed tile, it is returned
 * to the rack *)
and second_click_on_board st s' (letter, is_blank) rack_index np_tiles acc =
  (* cannot place a tile on top of a tile that was played on a previous turn *)
  let cell_coord = get_board_coord s' in
  let cell_letter = fst (get_cell_from_coordinate cell_coord st).letter in
  let np_letters = np_tiles |> List.split |> snd |> List.map fst in
  let is_newly_placed = List.mem cell_letter np_letters in
  if ((not is_newly_placed) && (cell_letter <> ' ')) then
    second_click_helper st (letter, is_blank) rack_index np_tiles acc
  else (* board has been clicked on an empty tile, or newly placed tile *)
    (* try returning tile to rack. if nothing there, nothing will happen *)
    let vb_index = (coord_to_array_index cell_coord) in
    let (st', np_tiles', acc') = return_to_rack st vb_index np_tiles acc in
    (* update player and rack based on removing *)
    let new_current_player = st'.current_player in
    let new_rack = new_current_player.rack in
    (* to account for the offset when a tile is added back to the rack *)
    let rack_index =
      if np_tiles = np_tiles' then rack_index else (rack_index + 1)
    in
    (* place tile on the board *)
    let r' = remove_from_rack (fst (List.nth new_rack rack_index)) new_rack in
    let rack_len = List.length r' in
    let r_array = rack rack_len in
    let new_current_player' = {new_current_player with rack = r'} in
    (* visually update rack *)
    erase_rack ();
    draw_rack new_current_player' r_array;
    (* update board state *)
    let mv = {word = [letter]; mv_coord = (cell_coord); is_horizontal = true} in
    let b' = State.place_horizontal mv st' in
    (* visually update board *)
    update_vb (List.flatten b');
    update_board (List.flatten b');
    (* shade newly placed tiles *)
    let np_tiles'' = (vb_index, (letter, is_blank)) :: np_tiles' in
    shade_np_tiles np_tiles'';
    let st'' = {st' with board = b'; current_player = new_current_player'} in
    let cell_index = ((cell_coord, letter), st'') in
    (* recursively call place_helper to get the next first click *)
    (place_helper st'' (np_tiles'') (cell_index :: acc'))

(* [second_click_on_rack st curr_idx new_idx np_tiles acc] accounts for the case
 * when the second place click occurs on the rack. If [curr_idx] does not equal
 * [new_idx], then the selected tile switches to the one at [new_idx] and a new
 * second click is received. Else, the selected tile becomes deselected, and a
 * new first click is received. *)
and second_click_on_rack st curr_idx new_idx np_tiles acc =
  (* selecting a new tile in rack, highlight that one instead *)
  if curr_idx <> new_idx then
    let dark_colors = (dark_beige1, dark_beige3, dark_beige2) in
    let letter, is_blank = (color_tile_get_letter st new_idx dark_colors) in
    (second_click_helper st (letter, is_blank) new_idx np_tiles acc)
  (* reclicking highlighted tile in rack, unhighlight it *)
  else
    let colors = (beige1, beige3, beige2) in
    let _ = (color_tile_get_letter st new_idx colors) in
    (place_helper st np_tiles acc)

(* [str_of_help ()] is a reversed list of strings of gui_help.txt *)
let rec str_lst_of_help () =
  let rec helper channel str =
    match (Pervasives.input_line channel) with
    | exception End_of_file -> Pervasives.close_in channel; [str]
    | s -> str :: helper channel (s)
  in
  helper (Pervasives.open_in "gui_help.txt") ""

(* [help_helper st] deals with the displaying of the help message in the gui *)
let help_helper st =
  set_color white;
  fill_rect 601 0 399 600;
  set_color black;
  draw_logo ();
  let done_btn = {x = 770; y = 20; w = 40; h = 40; bw = 2;
                b1_col = gray1; b2_col = gray3; b_col = gray2} in
  draw_box done_btn;
  draw_string_in_box "Done" done_btn Graphics.black;
  let h = snd (Graphics.text_size "|") in
  let help_str_lst = str_lst_of_help () in
  let _ = List.fold_left
      (fun acc s -> moveto 620 acc;
        Graphics.draw_string s; acc-h) 495 help_str_lst in
  let rec loop () =
    let s = wait_next_event [Button_down] in
    if mem (s.mouse_x, s.mouse_y) (770, 20, 40, 40) then ()
    else loop ()
  in loop ()

(* [show_if_hidden st] shows the current player's rack if it is hidden, and
 * does nothing if it is already shown. *)
let show_if_hidden st =
  if (rack_hidden 662 0 (List.length st.current_player.rack) true) then
    toggle_rack st.current_player
  else ()

(* [gui_cmd st] is the command received from user input via the gui *)
let rec gui_cmd st =
  let curr_status = wait_next_event [Button_down] in
  let x = curr_status.mouse_x in
  let y = curr_status.mouse_y in
  if mem (x, y) pass_btn then
    Pass
  else if mem (x, y) help_btn then
    (help_helper st; Help)
  else if mem (x, y) hint_btn then
    Hint
  else if mem (x, y) quit_btn then
    Quit
  else if mem (x, y) toggle_rack_btn then
    (toggle_rack st.current_player; Rack)
  else if mem (x, y) add_btn then
    AddWord (addword_helper st)
  else if mem (x, y) swap_btn then
    let () = show_if_hidden st in
      let b_swap = {x = 816; y = 120; w = 60; h = 60; bw = 2;
        b1_col = dark_gray1; b2_col = dark_gray3; b_col = dark_gray2}
      in
      draw_box b_swap;
      draw_string "Swap" 835 153 true;
      draw_string "Tiles" 832 135 true;
      Swap (swap_helper st.current_player)
  else if mem (x, y) place_btn then
    let () = show_if_hidden st in
    let () = dark_gray_place () in
    let mv = List.map (fun (f,_) -> f) (place_helper st [] []) in
    PlaceWord (convert_to_move mv st)
  else gui_cmd st

(* [init_gui st] initializes the GUI when the game starts with initial state
 * [st]. The graphics window is opened and the empty board, logo, scoreboard,
 * first player's rack, and buttons are drawn. *)
let init_gui st =
  try
    Graphics.open_graph " 1000x600";
    Graphics.set_window_title "Scrabble";
    Array.iter draw_box vb;
    update_scores (st.players);
    update_board (List.flatten st.board);
    draw_string_in_box "START" vb.(112) Graphics.black;
    draw_logo ();
    draw_buttons true;
    draw_string (st.current_player.name ^ "'s turn.") 770 470 true;
    draw_io_box ()
  with
  | Graphics.Graphic_failure("fatal I/O error") ->
    print_endline "Thanks for playing!"
