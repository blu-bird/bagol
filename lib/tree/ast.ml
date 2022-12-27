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