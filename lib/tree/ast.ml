open Token 

(**[expr] is the representation type of expressions. *)
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
| ELogic of token * expr * expr 
| ECall of expr * token * expr list 

type stmt = 
| SExpr of expr 
| SPrint of expr 
| SVarDecl of token * expr option 
| SBlock of stmt list
| SIf of expr * stmt * stmt option 
| SWhile of expr * stmt 
| SFun of token * token list * stmt list 
| SReturn of token * expr option

(**[format_expr] returns a string representation of an [expr].*)
let rec format_expr = function 
| EBool b -> string_of_bool b
| ENum f -> string_of_float f
| EStr s -> "\"" ^ s ^ "\""
| ENil -> "nil"
| EUnary (u, e) -> "(" ^ u.lexeme ^ " " ^ format_expr e ^ ")" 
| EBinary (b , e1 , e2) -> "(" ^ b.lexeme ^ " " ^ format_expr e1 ^ " " ^ format_expr e2 ^ ")"
| EGroup e -> "(group " ^ (format_expr e) ^ " )" 
| EVar t -> t.lexeme
| EAssign (t, e) -> "(assign " ^ t.lexeme ^ " " ^ format_expr e ^ ")"
| ELogic (b, e1, e2) -> "(logic-" ^ b.lexeme ^ " " ^ format_expr e1 ^ " " ^ format_expr e2 ^ ")"  
| ECall (e, _, _) -> "(call " ^ format_expr e ^ " on " ^ ")" 
