
build: 
	dune build 

file: 
	OCAMLRUNPARAM=b dune exec bin/tree/treeinterpreter.exe data/unexpected_character.lox

utop: 
	OCAMLRUNPARAM=b dune exec bin/tree/treeinterpreter.exe

test-scan: 
	OCAMLRUNPARAM=b dune exec test/scanner.exe

test-unexpected: 
	OCAMLRUNPARAM=b dune exec bin/tree/treeinterpreter.exe data/unexpected_character.lox

