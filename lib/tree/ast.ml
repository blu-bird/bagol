open Token 

type expr = 
| EBool of bool
| ENum of float
| EStr of string 
| ENil 
| EUnary of token * expr 
| EBinary of token * expr * expr 
| EGroup of expr
| EVar of token 
| EAssign of token * expr 

type stmt = 
| SExpr of expr 
| SPrint of expr 
| SVarDecl of token * expr option 

