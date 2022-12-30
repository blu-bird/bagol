open Errorhandling 
open Value
open Token 

module Env = Map.Make (String)

(**[env] is the representation type of environments in Bagol. *)
type env = {prev : env option; bindings : env value Env.t} 

let define s v env = {env with bindings = Env.add s v env.bindings}



let rec assign tok v env = 
  if Env.mem tok.lexeme env.bindings then {env with bindings = Env.add tok.lexeme v env.bindings}
  else match env.prev with 
  | None -> raise (RuntimeError (tok, "Undefined variable '" ^ tok.lexeme ^ "'."))
  | Some p -> {env with prev = Some (assign tok v p)}

let empty_bindings = Env.empty 

let initial_env = {prev = None; bindings = Env.empty}

let push_env env = {prev = Some env; bindings = empty_bindings}

(**[global_env] defines the built-in function [clock] that takes 0 arguments
    and returns [Sys.time ()] (time since program started executing) to  
    MAY NEED REWORK  *)
let global_env = define "clock"
  (VFunc {arity = 0; call = fun _ _ _ _ _ -> VNum (Sys.time ()), initial_env })
  initial_env

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

    let rec get tok env = 
      if Env.mem tok.lexeme env.bindings then Env.find tok.lexeme env.bindings
      else match env.prev with 
      | None -> raise (RuntimeError (tok, "Undefined variable '" ^ tok.lexeme ^ "'."))
      | Some p -> (* print_endline ("trying to resolve " ^ tok.lexeme ^ " in " ^ string_of_env env); **)
        get tok p