(* open Interpreter  *)
open Ast 
open Token 
open Errorhandling
open State 

module Scope = Map.Make(String)

type scope = bool Scope.t

type locals = (token, int) Hashtbl.t

type currEnclose = 
| Nothing 
| Function 

let scope_stack : scope Stack.t = Stack.create ()

let currLocals : locals ref = ref (Hashtbl.create 256) 

let begin_scope () = Stack.push (Scope.empty) scope_stack

let end_scope () = let _ = Stack.pop scope_stack in ()

let token_scoped t defined = 
  if Stack.is_empty scope_stack then () 
  else let top = Stack.pop scope_stack in 
    Stack.push (Scope.add t.lexeme defined top) scope_stack

let declare_token t = token_scoped t false 

let define_token t = token_scoped t true 

let resolve_tok_depth e n = Hashtbl.add (!currLocals) e n 

let resolve_local t = 
  Stack.fold (fun () map -> if Scope.mem t.lexeme map then resolve_tok_depth t 0 else ()) () scope_stack

let rec resolve_expr = function 
| EVar t -> resolve_var t 
| EAssign (t, e) -> resolve_assign t e 
| EBinary (_, e1, e2) -> resolve_expr e1; resolve_expr e2
| ECall (e, _, exprList) -> resolve_call e exprList
| EGroup e -> resolve_expr e
| ELogic (_, e1, e2) -> resolve_expr e1; resolve_expr e2
| EUnary (_, e) -> resolve_expr e
| EBool _ | ENum _ | EStr _ | ENil -> ()

and resolve_call e eList = 
  resolve_expr e; 
  List.fold_left (fun () e -> resolve_expr e) () eList 

and resolve_var t = 
  if not (Stack.is_empty scope_stack) && Scope.find_opt t.lexeme (Stack.top scope_stack)  = Some false then 
    error_token t "Can't read local variable in its own initializer."
  else resolve_local t 

and resolve_assign t e = 
  resolve_expr e; resolve_local t

let rec resolve_stmt = function 
| SBlock stmtList -> resolve_block stmtList
| SVarDecl (t, expr_opt) -> resolve_vardecl t expr_opt
| SFun (t, tl, sl) -> resolve_fun t tl sl 
| SExpr e -> resolve_expr e
| SIf (e, s, sOpt) -> resolve_if e s sOpt
| SPrint e -> resolve_expr e
| SReturn (_, eOpt) -> resolve_return eOpt 
| SWhile (e, s) -> resolve_while e s
(* | _ -> failwith "Unimplemented" *)

and resolve_while e s = 
  resolve_expr e; resolve_stmt s

and resolve_return = function 
| None -> ()
| Some e -> resolve_expr e

and resolve_if e s sOpt = 
  resolve_expr e; resolve_stmt s; 
  match sOpt with 
  | None -> () 
  | Some elseStmt -> resolve_stmt elseStmt

and resolve_fun t tl sl = 
  declare_token t; define_token t; resolve_fun_body tl sl; 

and resolve_fun_body tl sl = 
  begin_scope (); 
  List.fold_left (fun () t -> declare_token t; define_token t) () tl; 
  resolve_block sl; 
  end_scope () 

and resolve_vardecl tok expr_opt = 
  declare_token tok; match expr_opt with 
  | None -> ()
  | Some e -> resolve_expr e; 
  define_token tok; 

and resolve_block stmtList = 
  begin_scope (); List.fold_left (fun () s -> resolve_stmt s) () stmtList; end_scope ()

let resolve stmtList = List.fold_left (fun () s -> resolve_stmt s) () stmtList 


