# Changes made to Crafting Interpreters Implementations

## Tree-Walk Interpreter

### Chapter 4: Scanning 
- Factor out error-message handling into its own module to avoid cyclic dependencies
- **Remove tracking of string indices, instead consuming characters one at a time from a sequence of characters**
- Pattern-matching and refactoring of semantically similar cases
- **Options to indicate whether current consumption of characters produces a token or not (e.g. handling whitespace, comments)**
- Pull main executing function of interpreter into its own executing file
- Refactored recursive function with anonymous function instead of using while-loops 
- Renaming to avoid reserved words (e.g. `match`)
- Some changes to number test cases to align with behavior of `string_of_float`

### Chapter 5: Representing Code
As the textbook suggests, since we are programming in a functional 
language (thanks OCaml) we just leverage OCaml's type system
(in particular its variant and inductive types) to create the
representation type for the AST instead of using the visitor 
pattern as we would in an object-oriented language. 
