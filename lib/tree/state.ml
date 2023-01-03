open Errorhandling 
(* open Value *)
open Token 
open Ast 

module Env = Map.Make (String)

type value = 
| VNil 
| VBool of bool 
| VNum of float 
| VStr of string 
| VFunc of callable 

and env = {prev : env option; bindings : value Env.t} 

and callable = {arity : int; call : value list -> value; data : callData}

(* module type Callable = sig
  type t 
  val arity : int 
  val call : t value -> t value
end *)
and callData = 
| BuiltIn 
| Func of funcData 
     
and funcData = {decl : token * token list * stmt list ; mutable closure : env}

(* and func = {data : funcData option; callable : callable}  *)

let define s v env = {env with bindings = Env.add s v env.bindings}

let rec member s env = 
  if Env.mem s env.bindings then true 
  else match env.prev with 
  | None -> false 
  | Some p -> member s p

let rec get tok env = 
  if Env.mem tok.lexeme env.bindings then Env.find tok.lexeme env.bindings
  else match env.prev with 
  | None -> raise (RuntimeError (tok, "Undefined variable '" ^ tok.lexeme ^ "'."))
  | Some p -> get tok p

let rec assign tok v env = 
  if Env.mem tok.lexeme env.bindings then {env with bindings = Env.add tok.lexeme v env.bindings}
  else match env.prev with 
  | None -> raise (RuntimeError (tok, "Undefined variable '" ^ tok.lexeme ^ "'."))
  | Some p -> {env with prev = Some (assign tok v p)}

let rec assign_str s v env = 
  if Env.mem s env.bindings then {env with bindings = Env.add s v env.bindings}
  else match env.prev with 
  | None -> failwith ("Assigning to undefined variable '" ^ s ^ "'.")
  | Some p -> {env with prev = Some (assign_str s v p)}
let empty_bindings = Env.empty 
let empty_env = {prev = None; bindings = empty_bindings}

let push_env env = {prev = Some env; bindings = empty_bindings}

let pop_env env = match env.prev with | Some p -> p | None -> failwith "Runtime error: No previous env to evaluate ins"

let rec ancestor env d = 
  if d = 0 then env else ancestor (pop_env env) (d-1)

let getAt d env t = get t (ancestor env d)

let assignAt d env t v = assign t v (ancestor env d)

let globals = empty_env |> define "clock" (VFunc {data = BuiltIn; arity = 0; call = fun _ -> VNum (Sys.time ())}) 

let string_of_val = function 
| VNil -> "nil"
| VBool b -> string_of_bool b
| VNum f -> let sf = string_of_float f in 
  (if sf.[String.length sf - 1] = '.' then 
    String.sub sf 0 (String.length sf - 1)
  else sf)
| VStr s -> s
| VFunc f -> match f.data with 
  | Func fd -> let (tok, _, _) = fd.decl in Printf.sprintf "<fn %s>" tok.lexeme
  | _ -> failwith "not a function"

let rec string_of_bindings_help binds = 
  match binds with 
  | [] -> ""
  | (s,v) :: [] -> s ^ ": " ^ (string_of_val v)
  | (s', v') :: h' :: t -> s' ^ ": " ^ (string_of_val v') ^ ", " ^ string_of_bindings_help (h' :: t)

let string_of_bindings binds = 
  "[" ^ string_of_bindings_help binds ^ "]"

let rec string_of_env env = 
  "{prev: " ^ (match env.prev with None -> "None" | Some p -> string_of_env p) 
    ^ ", bindings: " ^ string_of_bindings (Env.bindings env.bindings) ^ "}"