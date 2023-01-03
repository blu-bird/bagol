(* open Errorhandling  *)
(* open Value *)
open Token 
open Ast 

(* module Env = Map.Make (String) *)

type value = 
| VNil 
| VBool of bool 
| VNum of float 
| VStr of string 
| VFunc of callable 

and env = {mutable prev : env option; bindings : (string, value) Hashtbl.t} 

and callable = {arity : int; call : value list -> value; data : callData}

(* module type Callable = sig
  type t 
  val arity : int 
  val call : t value -> t value
end *)
and callData = 
| BuiltIn of string
| Func of funcData 
     
and funcData = {decl : token * token list * stmt list ; mutable closure : env}

(* and func = {data : funcData option; callable : callable}  *)
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
  | BuiltIn s -> "<native " ^ s ^ ">"
  (* | _ -> failwith "not a function" *)

let rec string_of_bindings_help binds = 
  match binds with 
  | [] -> ""
  | (s,v) :: [] -> s ^ ": " ^ (string_of_val v)
  | (s', v') :: h' :: t -> s' ^ ": " ^ (string_of_val v') ^ ", " ^ string_of_bindings_help (h' :: t)

let string_of_bindings binds = 
  "[" ^ string_of_bindings_help binds ^ "]"

let rec string_of_env env = 
  "{prev: " ^ (match env.prev with None -> "None" | Some p -> string_of_env p) 
    ^ ", bindings: " ^ 
    string_of_bindings 
      (Hashtbl.fold (fun k v m -> if List.mem_assoc k m then m else (k, v) :: m) env.bindings []) ^ "}"



let empty_bindings = Hashtbl.create 256
let empty_env = {prev = None; bindings = empty_bindings}
let global_env = 
  let global_bindings = ref empty_bindings in 
  Hashtbl.add !global_bindings "clock" 
    (VFunc {data = BuiltIn "clock"; arity = 0; call = fun _ -> VNum (Sys.time ())}); 
  {prev = None; bindings = !global_bindings}
let curr_env = ref global_env

let define s v = Hashtbl.add !curr_env.bindings s v

let rec get_rec tok env = 
  if Hashtbl.mem env.bindings tok.lexeme then Hashtbl.find env.bindings tok.lexeme
  else match env.prev with 
  | None -> failwith ("Undefined variable '" ^ tok.lexeme ^ "' in environment " ^ string_of_env !curr_env )
  (* | None -> raise (RuntimeError (tok, "Undefined variable '" ^ tok.lexeme ^ "'.")) *)
  | Some p -> get_rec tok p

let get tok = get_rec tok !curr_env

let rec assign_rec tok v env = 
  if Hashtbl.mem env.bindings tok.lexeme then Hashtbl.add env.bindings tok.lexeme v 
  else match env.prev with 
  | None -> failwith ("Undefined variable '" ^ tok.lexeme ^ "'.")
  (* | None -> raise (RuntimeError (tok, "Undefined variable '" ^ tok.lexeme ^ "'.")) *)
  | Some p -> assign_rec tok v p 

let assign tok v = assign_rec tok v !curr_env 

let push_env () = curr_env := {prev = Some !curr_env; bindings = Hashtbl.create 256}

let pop_env () = match !curr_env.prev with | Some p -> curr_env := p; p | None -> failwith "Runtime error: No previous env"

let rec ancestor_help d env = 
  if d = 0 then env else 
  match env.prev with 
  | None -> failwith "Runtime error: Not enough ancestors"
  | Some p -> ancestor_help (d-1) p 

let ancestor d = ancestor_help d !curr_env

let getAt d t = get_rec t (ancestor d)

let assignAt d t v = assign_rec t v (ancestor d)

