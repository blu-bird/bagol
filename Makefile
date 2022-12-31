
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

test-statements: 
	OCAMLRUNPARAM=b dune exec bin/tree/treeinterpreter.exe data/basic_statements.lox
	
test-loops: 
	OCAMLRUNPARAM=b dune exec bin/tree/treeinterpreter.exe data/fibloops.bgl

test-funcs: 
	OCAMLRUNPARAM=b dune exec bin/tree/treeinterpreter.exe data/fibrecur.bgl

test-curr: 
	OCAMLRUNPARAM=b dune exec bin/tree/treeinterpreter.exe data/test.bgl

