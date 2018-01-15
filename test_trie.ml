open OUnit2
open Trie

(* [file_word_list file] is the list of words found in [file]. *)
let file_word_list file =
  let rec helper channel lst =
    match (Pervasives.input_line channel) with
    (* If End_of_file, close file and return idx' *)
    | exception End_of_file -> Pervasives.close_in channel ; lst
    | s -> helper channel (s::lst)
  in
  helper (Pervasives.open_in file) []

(* forward_dict.txt dictionary and list *)
let f_dict = initialize_dict "forward_dict.txt"
let f_list = file_word_list "forward_dict.txt"

(* reverse_dict.txt dictionary and list *)
let r_dict = initialize_dict "reverse_dict.txt"
let r_list = file_word_list "reverse_dict.txt"

(* words that should not be found in [f_dict] *)
let fake_words = [
  "a"; "aaa"; "abacterials"; "abac"; "abasjment"; "abb"; "abnormalityes";
  "abstemiousnessesp"; "123"; "/.,/()*&^%$#@!!~`"; " "; ""; " admissibilities";
  "admix "; "zyzzyvass"; "zyzz yvas"; "zymurgis"; "zygosity."
]

let tests = [
  "forward_dict" >:: (fun _ -> assert_equal f_list (List.filter (is_word f_dict) f_list));
  "reverse_dict" >:: (fun _ -> assert_equal r_list (List.filter (is_word r_dict) r_list));
  "fake_words" >:: (fun _ -> assert_equal [] (List.filter (is_word f_dict) fake_words));
]
