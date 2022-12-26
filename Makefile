
build: 
	dune build 

file: 
	OCAMLRUNPARAM=b dune exec bin/tree/treeinterpreter.exe data/hello.txt

utop: 
	OCAMLRUNPARAM=b dune exec bin/tree/treeinterpreter.exe

test: 
	OCAMLRUNPARAM=b dune exec test/main.exe
