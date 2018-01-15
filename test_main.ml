open OUnit2

let suite = "Scrabble test suite" >:::
  Test_trie.tests @
  Test_state.init_state_tests @
  Test_state.add_word_tests @
  Test_state.swap_tests @
  Test_state.place_tests @
  Test_state.pass_tests @
  Test_ai.tests

let _ = run_test_tt_main suite
