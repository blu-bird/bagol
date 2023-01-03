(* open Interpreter  *)
open Ast 
open Token 
open Errorhandling
open State 

module Scope = Map.Make(String)

type scope = bool Scope.t

type locals = (expr, int) Hashtbl.t

type currEnclose = 
| Nothing 
| Function of token

let scope_stack : scope Stack.t = Stack.create ()

let currLocals : locals ref = ref (Hashtbl.create 256) 

let string_of_scope scope = string_of_bindings_help (Scope.bindings scope) (fun x -> x) (string_of_bool)
  
let string_of_locals () = "[" ^ 
  string_of_bindings_help
    (Hashtbl.fold (fun k v m -> if List.mem_assoc k m then m else (k, v) :: m) !currLocals [])
    (format_expr) (string_of_int) ^ "]"

let string_of_scope_stack () = Stack.fold (fun s m -> s ^ string_of_scope m) "" scope_stack

let currEnc = ref Nothing 

type encloser = (token , currEnclose) Hashtbl.t

let currEncloser : encloser ref = ref (Hashtbl.create 256)

let string_of_currEnclose = function 
| Nothing -> "nothing"
| Function f -> "function " ^ string_of_token f 

let print_encloser () = Hashtbl.iter 
  (fun k v -> print_string (k.lexeme ^ ": " ^ string_of_currEnclose v ^ " ")) 
  !currEncloser; print_string "\n"

let begin_scope () = Stack.push (Scope.empty) scope_stack

let end_scope () = let _ = Stack.pop scope_stack in ()

(* let token_scoped t defined = 
  if Stack.is_empty scope_stack then () 
  else let top = Stack.pop scope_stack in 
    Stack.push (Scope.add t.lexeme defined top) scope_stack *)

let declare_token t = 
  (if Stack.is_empty scope_stack then () 
  else let top = Stack.pop scope_stack in 
    if Scope.mem t.lexeme top then 
      error_token t "Already a variable with this name in this scope."
    else Stack.push (Scope.add t.lexeme false top) scope_stack); 
      Hashtbl.add !currEncloser t !currEnc (* added line here *)
  (* token_scoped t false  *)

let define_token t = 
  if Stack.is_empty scope_stack then () 
  else let top = Stack.pop scope_stack in 
    Stack.push (Scope.add t.lexeme true top) scope_stack

let resolve_tok_depth e n = Hashtbl.add (!currLocals) e n 

let resolve_local e t = 
  let stack_seq = Stack.to_seq scope_stack in 
  Seq.iteri (fun i map -> if Scope.mem t.lexeme map then resolve_tok_depth e i else ()) stack_seq
  (* Stack.fold (fun () map -> if Scope.mem t.lexeme map then resolve_tok_depth e 0 else ()) () scope_stack *)

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
  else resolve_local (EVar t) t 

and resolve_assign t e = 
  resolve_expr e; resolve_local (EAssign (t, e)) t

let rec resolve_stmt = function 
| SBlock stmtList -> resolve_block stmtList
| SVarDecl (t, expr_opt) -> resolve_vardecl t expr_opt
| SFun (t, tl, sl) -> resolve_fun t tl sl 
| SExpr e -> resolve_expr e
| SIf (e, s, sOpt) -> resolve_if e s sOpt
| SPrint e -> resolve_expr e
| SReturn (tok, eOpt) -> resolve_return tok eOpt 
| SWhile (e, s) -> resolve_while e s
(* | _ -> failwith "Unimplemented" *)

and resolve_while e s = 
  resolve_expr e; resolve_stmt s

and resolve_return tok eOpt =
  if !currEnc = Nothing then 
    error_token tok "Can't return from top-level code"
  else match eOpt with 
  | None -> ()
  | Some e -> resolve_expr e

and resolve_if e s sOpt = 
  resolve_expr e; resolve_stmt s; 
  match sOpt with 
  | None -> () 
  | Some elseStmt -> resolve_stmt elseStmt

and resolve_fun t tl sl = 
  declare_token t; 
  (* print_string ("declared " ^ t.lexeme ^ "; "); print_encloser ();   *)
  define_token t; resolve_fun_body tl sl (Function t); 

and resolve_fun_body tl sl ce = 
  let enclosing = !currEnc in 
  currEnc := ce; 
  begin_scope (); 
  List.fold_left (fun () t -> declare_token t; define_token t) () tl; 
  resolve_block sl; 
  end_scope (); 
  currEnc := enclosing

and resolve_vardecl tok expr_opt = 
  declare_token tok; 
  (* print_string ("declared " ^ tok.lexeme ^ "; "); print_encloser ();  *)
  match expr_opt with 
  | None -> ()
  | Some e -> resolve_expr e; 
  define_token tok; 

and resolve_block stmtList = 
  begin_scope (); List.fold_left (fun () s -> resolve_stmt s) () stmtList; end_scope ()

let resolve stmtList = begin_scope(); List.fold_left (fun () s -> resolve_stmt s) () stmtList 


