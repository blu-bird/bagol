open Errorhandling 
open Value
open Token 

module Env = Map.Make (String)

type env = {prev : env option; bindings : value Env.t} 

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
  | Some p -> assign tok v p 

let empty_bindings = Env.empty 
let initial_env = {prev = None; bindings = Env.empty}