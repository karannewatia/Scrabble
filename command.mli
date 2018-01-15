(* [coordinate] represents the coordinate of a board cell. *)
type coordinate = int * int

(* [move] represents a move when a word is played on the board. *)
type move = {
  word : char list;
  mv_coord : coordinate;
  is_horizontal : bool;
}

(* [InvalidCommand] is raised when an entered command has the wrong number of
 * arguments or is not an understood command. *)
exception InvalidCommand

(* [command] represents a command input by a player. *)
type command =
  | PlaceWord of move (* place a word on the board *)
  | Swap of char list (* swap tiles with the bag *)
  | Rack (* show/hide current player's rack of tiles *)
  | Hint (* give a hint to the player *)
  | AddWord of string (* adds custom word to dictionary *)
  | Help (* tells the user what commands they can use to play the game *)
  | Quit (* quit the game *)
  | Pass (* make no move and end turn *)
