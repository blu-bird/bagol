open Errorhandling 
open Value
open Token 

module Env = Map.Make (String)

type env = value Env.t 

let define s v env = Env.add s v env 

let get tok env = 
  if Env.mem tok.lexeme env then Env.find tok.lexeme env 
  else raise (RuntimeError (tok, "Undefined variable '" ^ tok.lexeme ^ "'."))

let assign tok v env = 
  if Env.mem tok.lexeme env then Env.add tok.lexeme v env 
  else raise (RuntimeError (tok, "Undefined variable '" ^ tok.lexeme ^ "'."))

let initial_env = Env.empty 