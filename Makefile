compile:
	ocamlbuild -use-ocamlfind state.cmo main.cmo command.cmo ai.cmo trie.cmo test_main.cmo gui.cmo

test:
	ocamlbuild -use-ocamlfind test_main.byte && ./test_main.byte

play:
	ocamlbuild -use-ocamlfind main.byte && ./main.byte
