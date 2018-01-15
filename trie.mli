(* The type of the dictionary, which will be implemented later as a trie *)
type dictionary = Node of char * (dictionary list) * bool

(* [explode s] is [s] split into a list of its characters, in order. *)
val explode : string -> char list

(* [get_subtree c dict] is [None] if no direct child of [dict] contains [c].
 * If there is a direct child of [dict], [child], that contains [c],
 * it returns [Some child] *)
val get_subtree : char -> dictionary -> dictionary option

(* [insert w dict] is [dict] with [w] appended as a valid word in [dict] *)
val insert : dictionary -> string -> dictionary

(* [is_word w dict] is true iff [w] is as a valid word in [dict] *)
val is_word : dictionary -> string -> bool

(* [initialize_dict file] is the dictionary that results from adding all the
 * words in [file] to the empty dictionary.
 * requires: there is one word per line in [file] *)
val initialize_dict : string -> dictionary

(* [get_subtree_string str dict] is [None] if no direct child of
 * [dict] contains [str].
 * If there is a direct child of [dict], [child], that contains [str],
 * it returns [Some child] *)
val get_subtree_string : string -> dictionary -> dictionary option

(* [get_ extensions str dict] returns a list of all possible extensions of
 * [str] in [dict].
 * If no extensions are possible, then returns [].
 *)
val get_extensions : string -> dictionary -> string list
