open Value 
open Ast 
open Token

module Interpreter = struct

(** THIS IS THE WRONG REP TYPE!! NEED REWRITE TO AST TO HAVE OPERATORS BE TOKENS *)
exception RuntimeError of string 

let isTruthy = function 
| VNil | VBool false -> false 
| _ -> true 

(* let checkNumberOperand token = function 
| VNum _ -> () 
| _ -> raise (RuntimeError (token, "Operand must be a number.")) *)

let to_num = function 
| VNum f -> f 
| _ -> raise (RuntimeError ("Operand must be a number."))

let rec interpret = function 
| EBool b -> VBool b
| ENum f -> VNum f
| EStr s -> VStr s
| ENil -> VNil
| EGroup e -> interpret e 
| EUnary (u, e) -> eval_unop u e 
| EBinary (b, e1, e2) -> eval_binop b e1 e2

and eval_unop u e = 
  let right = interpret e in
  match u with 
  | UBang -> VBool (not (isTruthy right))
  | UMinus -> VNum (Float.neg (to_num right))

and eval_binop b e1 e2 = 
  let left = interpret e1 in 
  let right = interpret e2 in 
  match b with 
  | (BMinus | BSlash | BStar | BGreater | BGreaterEqual | BLess | BLessEqual) as arithop -> 
    let left_num = to_num left in 
    let right_num = to_num right in 
    (match arithop with 
    | BMinus -> VNum (left_num -. right_num)
    | BSlash -> VNum (left_num /. right_num)
    | BStar -> VNum (left_num *. right_num)
    | BGreater -> VBool (left_num > right_num)
    | BGreaterEqual -> VBool (left_num >= right_num)
    | BLess -> VBool (left_num < right_num)
    | BLessEqual -> VBool (left_num <= right_num)
    | _ -> raise (Failure "Unreachable branch."))
  | BPlus -> eval_plus left right
  | BEqualEqual -> VBool (left = right)
  | BBangEqual -> VBool (left <> right)

and eval_plus v1 v2 = 
  match (v1, v2) with 
  | VNum n1 , VNum n2 -> VNum (n1 +. n2)
  | VStr s1 , VStr s2 -> VStr (s1 ^ s2)
  | _, _ -> raise (Failure "Incompatible types.")




end