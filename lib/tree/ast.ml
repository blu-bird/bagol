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
| EVar t -> "(var " ^ string_of_token t ^ ")"
| EAssign (t, e) -> "(assign " ^ t.lexeme ^ " " ^ format_expr e ^ ")"
| ELogic (b, e1, e2) -> "(logic-" ^ b.lexeme ^ " " ^ format_expr e1 ^ " " ^ format_expr e2 ^ ")"  
| ECall (e, _, _) -> "(call " ^ format_expr e ^ " on " ^ ")" 

and format_stmt = function 
| SExpr e -> "(expr " ^ format_expr e ^ ")"
| SPrint e -> "(print " ^ format_expr e ^ ")"
| SVarDecl (t, eOpt) -> "(varDecl " ^ t.lexeme ^ (match eOpt with | None -> "nil" | Some e -> format_expr e) ^ ")"
| SBlock sl -> "(block [" ^ List.fold_left (fun s stmt -> s ^ " " ^ format_stmt stmt) "" sl ^ "])" 
| SIf (e, s, sOpt) -> "(if " ^ format_expr e ^ " then " ^ format_stmt s ^ 
  (match sOpt with | None -> "" | Some t -> " else " ^ format_stmt t ) ^ ")"
| SWhile (e, s) -> "(while " ^ format_expr e ^ " do " ^ format_stmt s ^ ")"
| SFun (t, tl, sl) -> "(fun " ^ t.lexeme 
  ^ " args [" ^ List.fold_left (fun s t -> s ^ t.lexeme ^ " ") "" tl ^ "] in "
  ^ List.fold_left (fun s stmt -> s ^ " " ^ format_stmt stmt) "" sl ^ ")"
| SReturn (_, eOpt) -> "(return " ^ match eOpt with 
  | None -> "nil)" | Some e -> format_expr e ^ ")"

let rec copy_expr expr = match expr with 
| EBool b -> EBool b
| ENum f -> ENum f
| EStr s -> EStr s 
| ENil -> ENil
| EUnary (t, e) -> EUnary (copy_tok t, copy_expr e) 
| EBinary (t, e1, e2) -> EBinary (copy_tok t, copy_expr e1, copy_expr e2) 
| EGroup e -> EGroup (copy_expr e)
| EVar t -> EVar (copy_tok t)
| EAssign (t, e) -> EAssign (copy_tok t, copy_expr e) 
| ELogic (t, e1, e2) -> ELogic (copy_tok t, copy_expr e1, copy_expr e2)
| ECall (e, t, eList) -> ECall (copy_expr e, copy_tok t, 
  List.map copy_expr eList)