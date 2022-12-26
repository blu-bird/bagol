# Changes made to Crafting Interpreters Implementations

## Tree-Walk Interpreter

### Scanner (Chapter 4)
- Factor out error-message handling into its own module to avoid cyclic dependencies
- **Remove tracking of string indices, instead consuming characters one at a time from a sequence of characters**
- Pattern-matching and refactoring of semantically similar cases
- **Options to indicate whether current consumption of characters produces a token or not (e.g. handling whitespace, comments)**
- Pull main executing function of interpreter into its own executing file
- Refactored recursive function with anonymous function instead of using while-loops 
- Renaming to avoid reserved words (e.g. `match`)
