open Command
open State

(* The 4 possible directions of a move *)
type direction = Left | Right | Up | Down

(* [reverse_str s ] reverses [s].
 *  If [s] is the empty string, then the empty string is returned *)
val reverse_str : string -> string

(* [up_cell c] returns [Some c'] if [c'] is the cell coordinate
 * directly above [c], and [None] if the cell coordinate directly above [c]
 * is out of bounds. *)
val up_cell : State.cell -> State.coordinate option

(* [left_cell c] returns [Some c'] if [c'] is the cell coordinate
 * directly to the left of [c], and [None] if the cell coordinate
 * directly to the left of [c] is out of bounds.
*)
val left_cell : State.cell -> State.coordinate option

(* [down_cell c] returns [Some c'] if [c'] is the cell coordinate
 * directly below [c], and [None] if the cell coordinate directly below [c]
 * is out of bounds.
 *)
val down_cell : State.cell -> State.coordinate option

(* [right_cell c] returns [Some c'] if [c'] is the cell coordinate
 * directly to the right of [c], and [None] if the cell coordinate
 * directly to the right of [c] is out of bounds.
*)
val right_cell : State.cell -> State.coordinate option

(* [get_all_adj_words c st] returns a list of length 4 in which
 * the first element is the word to the left of [c],
 * the second element is the word to the right of [c],
 * the third element is the word above [c],
 * the fourth element is the word below [c].
 * If the cell in the corresponding direction is out of bounds of the board,
 * the word is taken to be the empty string.
 * If instead of a word, there is a single letter, then that letter
 * is taken to be the word.
 *)
val get_all_adj_words : State.cell -> State.state -> string list

(* [get_hint st] is the first valid move that AI can find based on the
 * current state of the game.
 * Uses a much smaller dictionary for move generation.
 * Used for the 'hint' command,
 * and also used by the Easy AI for move generation.
 *)
val get_hint : State.state -> Command.command

(* [best_move st] is the AI's best choice of a move based on the
 * current state of the game.
 * Uses the full Scrabble dictionary for move generation.
 * Used by the Hard AI for move generation.
 *)
val best_move : State.state -> Command.command
