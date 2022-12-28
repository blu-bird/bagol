open Scanner
open Token
open Errorhandling
open Ast
open Parser

(**[print_tokens] prints all elements in [tokens].*)
let rec print_tokens tokens = 
  match tokens with 
  | [] -> ()
  | h :: [] -> print_endline (string_of_token h); flush stdout
  | h :: h' :: t -> print_endline (string_of_token h); print_tokens (h' :: t)

(**[format_expr] returns a string representation of an [expr].*)
let rec format_expr = function 
| EBool b -> string_of_bool b
| ENum f -> string_of_float f
| EStr s -> "\"" ^ s ^ "\""
| ENil -> "nil"
| EUnary (u, e) -> "( " ^ (string_of_unop u) ^ " " ^ (format_expr e) ^ " )" 
| EBinary (b , e1 , e2) -> "( " ^ (string_of_binop b) ^ " " ^ (format_expr e1) ^ " " ^ (format_expr e2) ^ " )"
| EGroup e -> "( " ^ (format_expr e) ^ " )" 

let run src = print_endline src; 
  let tokens = Scanner.scanTokens (String.to_seq src) in 
  print_tokens tokens; 
  let expr = Parser.parse tokens in 
  if !hadError then () else 
  print_endline (format_expr expr)
  
let runFile path =
  let file_channel = open_in path in 
  let code = In_channel.input_all file_channel in 
    run code; if !hadError then exit 65 else ()

let rec runPrompt () = 
  print_string "> "; flush stdout; 
  let line = input_line stdin in 
  if line = "" then () else (run line; hadError := false; runPrompt ())

