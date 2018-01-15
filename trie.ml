type dictionary = Node of char * (dictionary list) * bool

(* [explode s] is [s] split into a list of its characters, in order. *)
let explode s =
  let rec helper n lst =
    if n = 0 then lst
    else helper (n-1) (s.[n-1]::lst)
  in
  helper (String.length s) []

(* [remove_child n dict] is [dict] with [c] removed, where [c] is the character
 * of [n]. *)
let remove_child n dict =
  match n with
  | Node (c, _, _) -> List.filter (fun (Node (c', _, _) )->  c' <> c) dict

(* [add_child n dict] is [dict] with [n] appended as a child. If the character
 * of [n] is already contained in a direct child of [dict], [n] replaces that
 * child. *)
let add_child n dict =
  match dict with
  | Node (c, lst, b) -> Node (c, n::(remove_child n lst), b)

let get_subtree c dict =
  match dict with
  | Node (_, lst, _) -> List.find_opt (fun (Node (c', _, _) )->  c' = c) lst

let rec get_subtree_string s dict =
  if String.length s = 0 then Some dict
  else let c = String.get s 0 in
    match get_subtree c dict with
    | None -> None
    | Some d ->
      get_subtree_string (String.sub s 1 (String.length s-1 )) d

(* [get_ extensions_helper dict str acc] is [acc',str'], where
 * [acc'] is a list of all possible extensions;
 * [str'] is only used internally to implement the function recursively
 * and is not useful as an output.
 *)
let rec get_extensions_helper dict str acc =
  match dict with
  | Node (c, d, true) ->
    let c' = Char.escaped c in
    let new_acc = (str ^ c')::acc in
    begin
      match d with
      | [] -> new_acc,(str ^ c')
      | _ ->
        List.fold_left (fun (acc',str') x ->
            fst (get_extensions_helper x str' acc'),str' )
          (new_acc,(str ^ c')) (d)
    end
  | Node (c, d, false) ->
    let c' = Char.escaped c in
    begin
      match d with
      | [] -> acc,(str ^ c')
      | _ ->
        List.fold_left (fun (acc',str') x ->
            fst(get_extensions_helper x str' acc'),str' )
          (acc,(str ^ c')) (d)
    end

let rec get_extensions s dict =
  match get_subtree_string s dict with
  | None -> []
  | Some d ->
    match d with
    | Node (_,d',_) ->
      List.flatten(
      List.fold_left (fun acc x ->
          (fst (get_extensions_helper x "" [])::acc))
        [] d')

let insert dict w =
  (* [helper lst acc] is a recursive helper function to [insert] that
   * updates [acc] one character at a time. *)
  let rec helper lst acc =
    match lst with
    | [] -> acc (* no more characters to insert *)
    | h::t ->
      let new_node =
        match (get_subtree h acc) with
        | None ->
            (* child does not yet exist; add a new node with no children. t=[]
             * indicates whether we are at the last letter of a word or not. *)
            Node (h, [], t=[])
        | Some d' ->
            (* if we are at the last letter of a word *)
            if t = [] then let Node(c, dl, _) = d' in Node(c, dl, true)
            (* else use the original node found in the trie *)
            else d'
      in
      (* This line of code is the crux of this function. We first recursively
       * call [helper] using [t] (the remainder of [lst]), and [new_node] (the
       * node we should use to insert [t]). After this recursive call returns,
       * we add the result to [acc], updating the entire dictionary. *)
      add_child (helper t new_node) acc
  in
  let char_list = explode (String.lowercase_ascii w) in
  helper char_list dict

let is_word dict w =
  (* [helper lst acc] is a recursive helper function to [is_word] that
   * utilizes a char list instead of a string. *)
  let rec helper acc lst =
    match lst with
    | [] -> false (* empty string is not a word *)
    | c :: t ->
      begin
        (* search for [c] in the children of [acc] *)
        match (get_subtree c acc) with
        | None -> false (* [c] not found, meaning [w] is not a word *)
        | Some d' ->
            (* last letter, check bool to see if we are at the end of a word *)
            if (t = []) then let (Node(_, _, b)) = d' in b
            (* else, keep recursing down the trie *)
            else (helper d' t)
      end
  in
  let char_list = explode (String.lowercase_ascii w) in
  helper dict char_list

let initialize_dict file =
  (* [helper channel dict] is a helper function to [intialize_dict] that
   * iterates through a file line by line to construct a dictionary. *)
  let rec helper channel dict =
    match (Pervasives.input_line channel) with
    (* If End_of_file, close file and return idx' *)
    | exception End_of_file -> Pervasives.close_in channel ; dict
    | s -> helper channel (insert dict s)
  in
  (* [+] used to represent the head of the trie *)
  let root = Node ('+', [], false) in
  helper (Pervasives.open_in file) root
