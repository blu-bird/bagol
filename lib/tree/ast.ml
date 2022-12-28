type unop = 
| UMinus 
| UBang

type binop = 
| BEqualEqual
| BBangEqual
| BLess 
| BLessEqual
| BGreater
| BGreaterEqual
| BPlus
| BMinus
| BStar
| BSlash 

type expr = 
| EBool of bool
| ENum of float
| EStr of string 
| ENil 
| EUnary of unop * expr 
| EBinary of binop * expr * expr 
| EGroup of expr 

(**[string_of_unop] gives the string representation for a [unop]. *)
let string_of_unop = function 
| UMinus -> "-"
| UBang -> "!"

(**[string_of_binop] gives the string representation for a [binop]. *)
let string_of_binop = function 
| BEqualEqual -> "=="
| BBangEqual -> "!="
| BLess -> "<"
| BLessEqual -> "<="
| BGreater -> ">"
| BGreaterEqual -> ">="
| BPlus -> "+"
| BMinus -> "-"
| BStar -> "*"
| BSlash -> "/"