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
and 'a callInter = { arity : int ; 
  call : ('a -> stmt list -> 'a) -> (string -> 'a value -> 'a ->'a) -> ('a -> string) 
  -> ('a -> 'a) -> ('a -> 'a) -> 'a -> 'a callData -> 'a value list -> 'a value * 'a * 'a }

and 'a funcData = {decl : token * token list * stmt list; closure : 'a ref}

and 'a callData = 
| Nada
| Func of 'a funcData 

and 'a callable = 'a callInter * 'a callData

let function_call (tok, paramToks, body) cl = 
  ({arity = List.length paramToks; 
    call = fun interp def debug push pop start cd args -> 
      print_endline ("closure of " ^ tok.lexeme ^ ": " ^ debug cl); 
      let params = List.map (fun t -> t.lexeme) paramToks in 
      let argEnv = List.fold_left2 (fun e s v -> def s v e) (push cl) params args in 
      let outEnv = interp argEnv body in
      let nextCl = pop outEnv in 
      (* print_endline ("outEnv is: " ^ debug nextCl); *)
      (match cd with 
      | Func fd -> fd.closure := nextCl; 
      (* print_endline ("updated: " ^ debug (!(fd.closure))) *)
      | _ -> failwith "Not calling a function."); 
      (* let argEnvRec = def tok.lexeme (VFunc (function_call (tok, paramToks, body) closure)) argEnv in *)
      (* print_endline (tok.lexeme); *)
      (* let returnedEnv = def tok.lexeme (VFunc (function_call (tok, paramToks, body) outEnv)) start in *)
      (* print_endline (debug returnedEnv); *)
      VNil, start, nextCl}, 
    Func {decl = (tok, paramToks, body); closure = ref cl}
  )


let string_of_val = function 
| VNil -> "nil"
| VBool b -> string_of_bool b
| VNum f -> let sf = string_of_float f in 
  (if sf.[String.length sf - 1] = '.' then 
    String.sub sf 0 (String.length sf - 1)
  else sf)
| VStr s -> s 
| VFunc _ -> "<fn>" 
  (* ^ (match cd with 
    | Func f -> let t, _, _ = f.decl in t.lexeme 
    | _ -> failwith "Not a function" ) ^ ">" *)