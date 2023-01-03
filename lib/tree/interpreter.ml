open Ast 
open Token
open Errorhandling
open State
open Resolver

module Interpreter = struct

exception Return of value

let isTruthy = function 
| VNil | VBool false -> false 
| _ -> true 

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
| EVar tok -> lookup_variable tok (EVar tok) 
| EAssign (tok, e) -> assign_env tok e 
| ELogic (b, e1, e2) -> eval_logic b e1 e2 
| ECall (e, t, eList) -> eval_call e t eList 

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

and lookup_variable t e = 
  let dOpt = Hashtbl.find_opt !currLocals e in 
  (match dOpt with 
  | None -> get t 
  | Some d -> getAt d t)

and assign_env t e = 
  let v = eval_expr e in 
  let dOpt = Hashtbl.find_opt !currLocals e in 
  (match dOpt with 
  | None -> assign t v
  | Some d -> assignAt d t v); v 

and eval_logic b e1 e2 = 
  let left = eval_expr e1 in 
  match b.tokenType with 
  | OR -> if isTruthy left then left else eval_expr e2 
  | AND -> if not (isTruthy left) then left else eval_expr e2
  | _ -> raise (Failure "Not a recognized logical operator.")

and eval_call e t eList = 
  let callee = eval_expr e in 
  match callee with 
  | VFunc f -> 
    let valList = List.map (fun expr -> eval_expr expr) eList in 
    if f.arity <> List.length valList then 
      raise (RuntimeError (t, "Expected " ^ string_of_int (f.arity) ^ " arguments but got " ^ string_of_int (List.length valList) ^ "."))
    else (* inline the call -- all implementations of call() go here *)
      (match f.data with 
      | BuiltIn _ -> f.call valList
      | Func fdata -> let old_env = !curr_env in 
        try ( eval_fun_call fdata f.call valList old_env ) with 
        | Return retVal -> curr_env := old_env; retVal )
  | _ -> raise (RuntimeError (t, "Can only call functions and classes."))

and eval_fun_call fdata fcall vals oldEnv =
    let (_, paramToks, body) = fdata.decl in 
    curr_env := fdata.closure; push_env (); 
    let params = List.map (fun t -> t.lexeme) paramToks in 
    List.fold_left2 (fun () s v -> define s v) () params vals;
    eval_block body; 
    (* update the closure if needed *)
    let nextCl = pop_env () in
    fdata.closure <- nextCl; 
    curr_env := oldEnv; 
    fcall vals

and eval_block stmtList = 
  push_env (); 
  List.fold_left (fun () stmt -> eval_stmt stmt) () stmtList; 
  let _ = pop_env () in ()

and eval_vardecl tok = function 
| None -> define tok.lexeme VNil 
| Some e -> let v = eval_expr e in define tok.lexeme v 
 
and eval_if e st seOpt = 
  let v = eval_expr e in 
  if isTruthy v then 
    eval_stmt st 
  else match seOpt with
  | None -> ()
  | Some se -> eval_stmt se 

and eval_while e s = 
  let v = eval_expr e in 
  if isTruthy v then 
    (eval_stmt s; eval_while e s)
  else ()

and eval_fun tok params body = 
  let funcData = {decl = (tok, params, body); closure = !curr_env} in 
  let funval = VFunc {arity = List.length params; call = (fun _ -> VNil); data = Func funcData} in 
  define tok.lexeme funval; 
  funcData.closure <- !curr_env; 

and eval_return _ exprOpt = 
  let returnValue = (match exprOpt with 
  | None -> VNil
  | Some e -> eval_expr e) in 
  raise (Return returnValue)

and eval_stmt = function  
| SExpr e -> let _ = eval_expr e in ()
| SPrint e -> let value = eval_expr e in print_endline (string_of_val value); flush stdout
| SVarDecl (tok, expr_opt) -> eval_vardecl tok expr_opt
| SBlock stmtList -> eval_block stmtList 
| SIf (e, st, seOpt) -> eval_if e st seOpt
| SWhile (e, s) -> eval_while e s
| SFun (tok, params, body) -> eval_fun tok params body 
| SReturn (tok, expr) -> eval_return tok expr 

let interpret stmtList = 
  (* STATEMENTS *)
  try (
    let _ = List.fold_left (fun () stmt -> eval_stmt stmt) () stmtList in ()
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