open Value 
open Ast 
open Token
open Errorhandling
open Environment

module Interpreter = struct

exception Return of env value

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
| EAssign (tok, e) -> let v, env' = eval_expr env e in 
  v, assign tok v env'
| ELogic (b, e1, e2) -> eval_logic b env e1 e2 
| ECall (e, t, eList) -> eval_call env e t eList 

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

and eval_logic b env e1 e2 = 
  let left, env' = eval_expr env e1 in 
  match b.tokenType with 
  | OR -> if isTruthy left then left, env' else eval_expr env' e2 
  | AND -> if not (isTruthy left) then left, env' else eval_expr env' e2
  | _ -> raise (Failure "Not a recognized logical operator.")

and eval_call env e t eList = 
  let callee, env' = eval_expr env e in 
  let envArgs, valList = 
    List.fold_left_map 
      (fun envIn expr -> let v, envOut = eval_expr envIn expr in envOut, v) env' eList in 
  match callee with 
  | VFunc call -> 
      if (List.length valList <> call.arity) then 
        raise (RuntimeError (t, "Expected " ^ string_of_int call.arity 
        ^ " arguments but got " ^ string_of_int (List.length valList) ^ "."))
      else (try (call.call (eval_block) (define) (push_env) envArgs valList) with 
      | Return rval -> rval, envArgs 
      | exn -> raise exn)
  | _ -> raise (RuntimeError (t, "Can only call functions and classes."))

and eval_block env stmtList = 
  let blockEnv = {prev = Some env; bindings = empty_bindings} in 
  let endEnv = List.fold_left (fun e stmt -> eval_stmt e stmt) blockEnv stmtList in
  match endEnv.prev with 
    None -> failwith ("Impossible exiting block env: " ^ (string_of_env endEnv)) | Some p -> p

and eval_vardecl env tok = function 
| None -> define tok.lexeme VNil env 
| Some e -> let v, env' = eval_expr env e in define tok.lexeme v env'

and eval_if env e st seOpt = 
  let v, env' = eval_expr env e in 
  if isTruthy v then 
    eval_stmt env' st 
  else match seOpt with
  | None -> env' 
  | Some se -> eval_stmt env' se 

and eval_while env e s = 
  let v, env' = eval_expr env e in 
  if isTruthy v then 
    let envb = eval_stmt env' s in 
    eval_while envb e s 
  else env'

and eval_fun env (tok, tokList, body) = 
  let func = VFunc (function_call (tok, tokList, body) env) in 
  define tok.lexeme func env 

and eval_return env _ exprOpt = 
  let returnValue = (match exprOpt with 
  | None -> VNil
  | Some e -> fst (eval_expr env e)) in 
  raise (Return returnValue)

and eval_stmt env = function  
| SExpr e -> let _, env' = eval_expr env e in env' 
| SPrint e -> (* print_endline (string_of_env env); *) let value, env' = eval_expr env e in (* print_endline (string_of_env env'); *)print_endline (string_of_val value); flush stdout; env' 
| SVarDecl (tok, expr_opt) -> eval_vardecl env tok expr_opt
| SBlock stmtList -> eval_block env stmtList 
| SIf (e, st, seOpt) -> eval_if env e st seOpt
| SWhile (e, s) -> eval_while env e s
| SFun (tok, tokList, body) -> eval_fun env (tok, tokList, body)
| SReturn (tok, exprOpt) -> eval_return env tok exprOpt 

let interpret stmtList = 
  (* STATEMENTS *)
  try (
    let _ = List.fold_left (fun e stmt -> eval_stmt e stmt) initial_env stmtList in ()
  ) with 
  | RuntimeError (t, msg) -> runtimeError (RuntimeError (t, msg))
  | Failure s -> raise (Failure ("Unhandled runtime error. " ^ s))
  
  (* EXPRESSIONS
  args: expr : expr
  try ( 
    let value = eval_expr expr in 
    print_endline(string_of_val value)
  ) with 
  | RuntimeError (t, msg) -> runtimeError (RuntimeError (t, msg))
  | _ -> raise (Failure "Unhandled runtime error.") *)

end