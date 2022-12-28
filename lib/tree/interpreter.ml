open Value 
open Ast 
open Token
open Errorhandling

module Interpreter = struct

let isTruthy = function 
| VNil | VBool false -> false 
| _ -> true 

(* let checkNumberOperand token = function 
| VNum _ -> () 
| _ -> raise (RuntimeError (token, "Operand must be a number.")) *)

let to_num_unary token = function 
| VNum f -> f 
| _ -> raise (RuntimeError (token, "Operand must be a number."))

let to_num_binary token = function
| VNum f1, VNum f2 -> f1 , f2 
| _ -> raise (RuntimeError (token, "Operands must be numbers."))

let rec eval_expr = function 
| EBool b -> VBool b
| ENum f -> VNum f
| EStr s -> VStr s
| ENil -> VNil
| EGroup e -> eval_expr e 
| EUnary (u, e) -> eval_unop u e 
| EBinary (b, e1, e2) -> eval_binop b e1 e2

and eval_unop u e = 
  let right = eval_expr e in
  match u.tokenType with 
  | BANG -> VBool (not (isTruthy right))
  | MINUS -> VNum (Float.neg (to_num_unary u right))
  | _ -> raise (Failure "Parser violated Bagol grammar (unary).")

and eval_binop b e1 e2 = 
  let left = eval_expr e1 in 
  let right = eval_expr e2 in 
  match b.tokenType with 
  | (MINUS | SLASH | STAR | GREATER | GREATER_EQUAL | LESS | LESS_EQUAL) as arithop -> 
    let left_num , right_num = to_num_binary b (left, right) in 
    (match arithop with 
    | MINUS -> VNum (left_num -. right_num)
    | SLASH -> VNum (left_num /. right_num)
    | STAR -> VNum (left_num *. right_num)
    | GREATER -> VBool (left_num > right_num)
    | GREATER_EQUAL -> VBool (left_num >= right_num)
    | LESS -> VBool (left_num < right_num)
    | LESS_EQUAL -> VBool (left_num <= right_num)
    | _ -> raise (Failure "Unreachable branch."))
  | PLUS -> eval_plus b left right
  | EQUAL_EQUAL -> VBool (left = right)
  | BANG_EQUAL -> VBool (left <> right)
  | _ -> raise (Failure "Unimplemented binary operation.")

and eval_plus b v1 v2 = 
  match (v1, v2) with 
  | VNum n1 , VNum n2 -> VNum (n1 +. n2)
  | VStr s1 , VStr s2 -> VStr (s1 ^ s2)
  | _, _ -> raise (RuntimeError (b, "Operands must be two numbers or two strings."))

let interpret expr = 
  try (
    let value = eval_expr expr in 
    print_endline(string_of_val value)
  ) with 
  | RuntimeError (t, msg) -> runtimeError (RuntimeError (t, msg))
  | _ -> raise (Failure "Unhandled runtime error.")

end