type coordinate = int * int

type move = {
  word : char list;
  mv_coord : coordinate;
  is_horizontal : bool;
}

exception InvalidCommand

type command =
  | PlaceWord of move
  | Swap of char list
  | Rack
  | Hint
  | AddWord of string
  | Help
  | Quit
  | Pass 
