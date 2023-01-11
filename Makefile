
build: 
	dune build 

file: 
	OCAMLRUNPARAM=b dune exec bin/tree/treeinterpreter.exe data/unexpected_character.lox

utop: 
	OCAMLRUNPARAM=b dune exec bin/tree/treeinterpreter.exe

test-scan: 
	OCAMLRUNPARAM=b dune exec test/scan/scanner.exe

test-unexpected: 
	OCAMLRUNPARAM=b dune exec bin/tree/treeinterpreter.exe data/unexpected_character.lox

test-statements: 
	OCAMLRUNPARAM=b dune exec bin/tree/treeinterpreter.exe data/basic_statements.lox
	
test-loops: 
	OCAMLRUNPARAM=b dune exec bin/tree/treeinterpreter.exe data/fibloops.bgl

test-curr: 
	OCAMLRUNPARAM=b dune exec bin/tree/treeinterpreter.exe data/for/scope.lox

test-assign: 
	OCAMLRUNPARAM=b dune exec bin/tree/treeinterpreter.exe data/assignment/associativity.lox
	OCAMLRUNPARAM=b dune exec bin/tree/treeinterpreter.exe data/assignment/global.lox
	# OCAMLRUNPARAM=b dune exec bin/tree/treeinterpreter.exe data/assignment/grouping.lox
	# OCAMLRUNPARAM=b dune exec bin/tree/treeinterpreter.exe data/assignment/infix_operator.lox
	OCAMLRUNPARAM=b dune exec bin/tree/treeinterpreter.exe data/assignment/local.lox
	# OCAMLRUNPARAM=b dune exec bin/tree/treeinterpreter.exe data/assignment/prefix_operator.lox
	OCAMLRUNPARAM=b dune exec bin/tree/treeinterpreter.exe data/assignment/syntax.lox
	# OCAMLRUNPARAM=b dune exec bin/tree/treeinterpreter.exe data/assignment/to_this.lox
	# OCAMLRUNPARAM=b dune exec bin/tree/treeinterpreter.exe data/assignment/undefined.lox

test-block: 
	OCAMLRUNPARAM=b dune exec bin/tree/treeinterpreter.exe data/block/empty.lox
	OCAMLRUNPARAM=b dune exec bin/tree/treeinterpreter.exe data/block/scope.lox

test-bool: 
	OCAMLRUNPARAM=b dune exec bin/tree/treeinterpreter.exe data/bool/equality.lox
	OCAMLRUNPARAM=b dune exec bin/tree/treeinterpreter.exe data/bool/not.lox

test-call: 
	# OCAMLRUNPARAM=b dune exec bin/tree/treeinterpreter.exe data/call/bool.lox
	# OCAMLRUNPARAM=b dune exec bin/tree/treeinterpreter.exe data/call/nil.lox
	# OCAMLRUNPARAM=b dune exec bin/tree/treeinterpreter.exe data/call/num.lox
	# OCAMLRUNPARAM=b dune exec bin/tree/treeinterpreter.exe data/call/object.lox
	# OCAMLRUNPARAM=b dune exec bin/tree/treeinterpreter.exe data/call/string.lox

test-closure: 
	OCAMLRUNPARAM=b dune exec bin/tree/treeinterpreter.exe data/closureTest.bgl
	OCAMLRUNPARAM=b dune exec bin/tree/treeinterpreter.exe data/closure/assign_to_closure.lox
	OCAMLRUNPARAM=b dune exec bin/tree/treeinterpreter.exe data/closure/assign_to_shadowed_later.lox
	OCAMLRUNPARAM=b dune exec bin/tree/treeinterpreter.exe data/closure/close_over_function_parameter.lox
	OCAMLRUNPARAM=b dune exec bin/tree/treeinterpreter.exe data/closure/close_over_later_variable.lox
	# OCAMLRUNPARAM=b dune exec bin/tree/treeinterpreter.exe data/closure/close_over_method_parameter.lox
	OCAMLRUNPARAM=b dune exec bin/tree/treeinterpreter.exe data/closure/closed_closure_in_function.lox
	OCAMLRUNPARAM=b dune exec bin/tree/treeinterpreter.exe data/closure/nested_closure.lox
	OCAMLRUNPARAM=b dune exec bin/tree/treeinterpreter.exe data/closure/open_closure_in_function.lox
	OCAMLRUNPARAM=b dune exec bin/tree/treeinterpreter.exe data/closure/reference_closure_multiple_times.lox
	OCAMLRUNPARAM=b dune exec bin/tree/treeinterpreter.exe data/closure/reuse_closure_slot.lox
	OCAMLRUNPARAM=b dune exec bin/tree/treeinterpreter.exe data/closure/shadow_closure_with_local.lox
	OCAMLRUNPARAM=b dune exec bin/tree/treeinterpreter.exe data/closure/unused_closure.lox
	OCAMLRUNPARAM=b dune exec bin/tree/treeinterpreter.exe data/closure/unused_later_closure.lox

test-comments:
	OCAMLRUNPARAM=b dune exec bin/tree/treeinterpreter.exe data/comments/line_at_eof.lox
	OCAMLRUNPARAM=b dune exec bin/tree/treeinterpreter.exe data/comments/only_line_comment_and_line.lox
	OCAMLRUNPARAM=b dune exec bin/tree/treeinterpreter.exe data/comments/only_line_comment.lox
	OCAMLRUNPARAM=b dune exec bin/tree/treeinterpreter.exe data/comments/unicode.lox

test-for: 
	# OCAMLRUNPARAM=b dune exec bin/tree/treeinterpreter.exe data/for/class_in_body.lox
	OCAMLRUNPARAM=b dune exec bin/tree/treeinterpreter.exe data/for/closure_in_body.lox
	# OCAMLRUNPARAM=b dune exec bin/tree/treeinterpreter.exe data/for/fun_in_body.lox
	OCAMLRUNPARAM=b dune exec bin/tree/treeinterpreter.exe data/for/return_closure.lox
	OCAMLRUNPARAM=b dune exec bin/tree/treeinterpreter.exe data/for/return_inside.lox
	OCAMLRUNPARAM=b dune exec bin/tree/treeinterpreter.exe data/for/scope.lox
	# OCAMLRUNPARAM=b dune exec bin/tree/treeinterpreter.exe data/for/statement_condition.lox
	# OCAMLRUNPARAM=b dune exec bin/tree/treeinterpreter.exe data/for/statement_increment.lox
	# OCAMLRUNPARAM=b dune exec bin/tree/treeinterpreter.exe data/for/statement_initializer.lox
	OCAMLRUNPARAM=b dune exec bin/tree/treeinterpreter.exe data/for/syntax.lox
	# OCAMLRUNPARAM=b dune exec bin/tree/treeinterpreter.exe data/for/var_in_body.lox

test-funcs: 
	OCAMLRUNPARAM=b dune exec bin/tree/treeinterpreter.exe data/fibrecur.bgl
	# OCAMLRUNPARAM=b dune exec bin/tree/treeinterpreter.exe data/function/body_must_be_block.lox
	OCAMLRUNPARAM=b dune exec bin/tree/treeinterpreter.exe data/function/empty_body.lox
	# OCAMLRUNPARAM=b dune exec bin/tree/treeinterpreter.exe data/function/extra_arguments.lox
	# OCAMLRUNPARAM=b dune exec bin/tree/treeinterpreter.exe data/function/local_mutual_recursion.lox
	OCAMLRUNPARAM=b dune exec bin/tree/treeinterpreter.exe data/function/local_recursion.lox
	# OCAMLRUNPARAM=b dune exec bin/tree/treeinterpreter.exe data/function/missing_arguments.lox
	# OCAMLRUNPARAM=b dune exec bin/tree/treeinterpreter.exe data/function/missing_comma_in_parameters.lox
	OCAMLRUNPARAM=b dune exec bin/tree/treeinterpreter.exe data/function/mutual_recursion.lox
	# OCAMLRUNPARAM=b dune exec bin/tree/treeinterpreter.exe data/function/nested_call_with_arguments.lox
	OCAMLRUNPARAM=b dune exec bin/tree/treeinterpreter.exe data/function/parameters.lox
	OCAMLRUNPARAM=b dune exec bin/tree/treeinterpreter.exe data/function/print.lox
	OCAMLRUNPARAM=b dune exec bin/tree/treeinterpreter.exe data/function/recursion.lox
	# OCAMLRUNPARAM=b dune exec bin/tree/treeinterpreter.exe data/function/too_many_arguments.lox
	# OCAMLRUNPARAM=b dune exec bin/tree/treeinterpreter.exe data/function/too_many_parameters.lox


test-interpreter: 
	dune runtest