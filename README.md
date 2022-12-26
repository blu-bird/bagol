# Bagol

Bagol is a variant of the Lox programming language as found in [Crafting Interpreters](http://craftinginterpreters.com/) 
implemented in OCaml. Very middling progress has been made thus far on the interpreter, 
but the plan is to eventually also write an implementation of the bytecode VM that 
simulates the limitations of C as well. 

## Goals 
- Learn how compilers and interpreters work! 
- Gain familiarity with dune, OCaml's build system.
- Write [correct, efficient, and beautiful](https://cs3110.github.io/textbook/cover.html) code.

## Implementation 
The implementation is very close to the textbook's implementation in Java 
(insert "Hey, can I copy your homework?" meme here). However, in some places we take 
advantage of some of OCaml's functional language features, so these implementations 
in the text do diverge from the main text in certain places. 
A "changelog" of what parts of the implementation I changed are listed in `changelog.md`.

## Testing 
We use the OUnit2 module to do unit testing. Currently, scanner tests are 
implemented and can be run with `make test-scan`. You can also test handling of 
unexpected characters with `make test-unexpected` which runs the tree-walk interpreter
on `unexpected_character.lox`. 

