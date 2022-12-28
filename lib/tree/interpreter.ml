open Value 
open Ast 
open Token
open Errorhandling
open Environment

module Interpreter = struct

let isTruthy = function 
| VNil | VBool false -> false 
| _ -> true 

let to_num_unary token = function 
| VNum f -> f 
| _ -> raise (RuntimeError (token, "Operand must be a number."))

let to_num_binary token = function
| VNum f1, VNum f2 -> f1 , f2 
| _ -> raise (RuntimeError (token, "Operands must be numbers."))

let rec eval_expr env = function 
| EBool b -> VBool b, env
| ENum f -> VNum f, env
| EStr s -> VStr s, env
| ENil -> VNil, env
| EGroup e -> eval_expr env e
| EUnary (u, e) -> eval_unop u env e
| EBinary (b, e1, e2) -> eval_binop b env e1 e2
| EVar tok -> get tok env, env 
| EAssign (tok, e) -> let v, env' = eval_expr env e in v, assign tok v env' 


and eval_unop u env e = 
  let right, env' = eval_expr env e in 
  match u.tokenType with 
  | BANG -> VBool (not (isTruthy right)), env' 
  | MINUS -> VNum (Float.neg (to_num_unary u right)), env' 
  | _ -> raise (Failure "Parser violated Bagol grammar (unary).")

and eval_binop b env e1 e2 = 
  let left, env' = eval_expr env e1 in 
  let right, env'' = eval_expr env' e2 in 
  match b.tokenType with 
  | (MINUS | SLASH | STAR | GREATER | GREATER_EQUAL | LESS | LESS_EQUAL) as arithop -> 
    let left_num , right_num = to_num_binary b (left, right) in 
    (match arithop with 
    | MINUS -> VNum (left_num -. right_num), env'' 
    | SLASH -> VNum (left_num /. right_num), env''
    | STAR -> VNum (left_num *. right_num), env''
    | GREATER -> VBool (left_num > right_num), env''
    | GREATER_EQUAL -> VBool (left_num >= right_num), env''
    | LESS -> VBool (left_num < right_num), env''
    | LESS_EQUAL -> VBool (left_num <= right_num), env''
    | _ -> raise (Failure "Unreachable branch."))
  | PLUS -> eval_plus b env'' left right
  | EQUAL_EQUAL -> VBool (left = right), env''
  | BANG_EQUAL -> VBool (left <> right), env''
  | _ -> raise (Failure "Unimplemented binary operation.")

and eval_plus b env v1 v2 = 
  match (v1, v2) with 
  | VNum n1 , VNum n2 -> VNum (n1 +. n2), env
  | VStr s1 , VStr s2 -> VStr (s1 ^ s2), env
  | _, _ -> raise (RuntimeError (b, "Operands must be two numbers or two strings."))

let eval_stmt env = function 
| SExpr e -> let _ = eval_expr env e in (), env
| SPrint e -> let value, env' = eval_expr env e in print_endline (string_of_val value); flush stdout, env' 
| SVarDecl (tok, expr_opt) -> (), match expr_opt with 
    | None -> define tok.lexeme VNil env 
    | Some e -> let v, env' = eval_expr env e in define tok.lexeme v env' 


let interpret stmtList = 
  (* STATEMENTS *)
  try (
    List.fold_left (fun ((), e) stmt -> eval_stmt e stmt) ((), initial_env) stmtList
  ) with 
  | RuntimeError (t, msg) -> runtimeError (RuntimeError (t, msg)), initial_env
  | _ -> raise (Failure "Unhandled runtime error.")

  (* EXPRESSIONS
  args: expr : expr
  try ( 
    let value = eval_expr expr in 
    print_endline(string_of_val value)
  ) with 
  | RuntimeError (t, msg) -> runtimeError (RuntimeError (t, msg))
  | _ -> raise (Failure "Unhandled runtime error.") *)

end