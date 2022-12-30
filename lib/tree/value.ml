open Ast 
open Token 

(**[value] is the representation type for values in Bagol. *)
type 'a value = 
| VNil 
| VBool of bool 
| VNum of float 
| VStr of string 
| VFunc of 'a callable 

(**['a callable] is the representation type for callable values in Bagol. This contains
    a [call] function that takes in an interpreting function of type ...
    MAY NEED A REWORK -- stmt instead of stmt list? 
    ['a] should ALWAYS be [env] *)
and 'a callable = { arity : int ; 
  call : ('a -> stmt list -> 'a) -> (string -> 'a value -> 'a ->'a) ->
    ('a -> 'a) -> 'a -> 'a value list -> 'a value * 'a }

and 'a func = (token * token list * stmt list) -> 'a -> 'a callable

let rec function_call (tok, paramToks, body) closure = 
  {arity = List.length paramToks; 
  call = fun interp def push start args -> 
    let startEnv = push closure in 
    let params = List.map (fun t -> t.lexeme) paramToks in 
    let argEnv = List.fold_left2 (fun e s v -> def s v e) startEnv params args in 
    let argEnvRec = def tok.lexeme (VFunc (function_call (tok, paramToks, body) closure)) argEnv in
    let _ = interp argEnvRec body in 
    VNil, start }


let string_of_val = function 
| VNil -> "nil"
| VBool b -> string_of_bool b
| VNum f -> let sf = string_of_float f in 
  (if sf.[String.length sf - 1] = '.' then 
    String.sub sf 0 (String.length sf - 1)
  else sf)
| VStr s -> s 
| _ -> ""