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

### Chapter 6: Parsing Expressions
- Refactored unary/binary expression parsing into recursive functions without while-loops
- **Use higher-order functions to refactor all left-associative binary operations**
- Use an `EBool <bool>` expression for booleans instead of separate true/false expressions
- **UNIMPLEMENTED: synchronization of the parser upon an error**

### Chapter 7: Evaluating Expressions
- **Mutually-recursive pattern-matching functions instead of visitor pattern**
- **Combine checking for validity of casting to integer and casting function together**
- **Use of variant type `value` to capture possible Bagol values (no Java `Object`)**
- Use OCaml's structural equality `=` to check equality 

### Chapter 8: Statements and State 
- **Environments must be passed as arguments to statement/expression evaluation**
  - DO YOU ACTUALLY WANT THIS? OR DO YOU WANT A REF? 
- Refactoring while loops as (mutually) recursive functions 

***(WE NEED MORE UNIT TESTS!!)***

### Chapter 9: Control Flow

### Chapter 10: Functions 

### Chapter 11: Resolving and Binding

### Chapter 12: Classes 

### Chapter 13: Inheritance 