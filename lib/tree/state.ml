open Errorhandling 
(* open Value *)
open Token 

module Env = Map.Make (String)

type value = 
| VNil 
| VBool of bool 
| VNum of float 
| VStr of string 
| VFunc of callable

and env = {prev : env option; bindings : value Env.t} 

and callable = {arity : int; call : value list -> value}

let define s v env = {env with bindings = Env.add s v env.bindings}

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

let empty_bindings = Env.empty 
let empty_env = {prev = None; bindings = Env.empty}

let globals = empty_env |> define "clock" (VFunc {arity = 0; call = fun _ -> VNum (Sys.time ())}) 

let string_of_val = function 
| VNil -> "nil"
| VBool b -> string_of_bool b
| VNum f -> let sf = string_of_float f in 
  (if sf.[String.length sf - 1] = '.' then 
    String.sub sf 0 (String.length sf - 1)
  else sf)
| VStr s -> s 
| VFunc _ -> ""

let rec string_of_bindings_help binds = 
  match binds with 
  | [] -> ""
  | (s,v) :: [] -> s ^ ": " ^ (string_of_val v)
  | (s', v') :: h' :: t -> s' ^ ": " ^ (string_of_val v') ^ string_of_bindings_help (h' :: t)

let string_of_bindings binds = 
  "[" ^ string_of_bindings_help binds ^ "]"

let rec string_of_env env = 
  "{prev: " ^ (match env.prev with None -> "None" | Some p -> string_of_env p) 
    ^ " bindings: " ^ string_of_bindings (Env.bindings env.bindings) ^ "}"