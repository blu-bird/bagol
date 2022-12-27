open Scanner
open Token
open Errorhandling
open Ast
open Parser

let rec print_tokens tokens = 
  match tokens with 
  | [] -> ()
  | h :: [] -> print_endline (string_of_token h); 
  | h :: h' :: t -> print_endline (string_of_token h); print_tokens (h' :: t)

let print_token_list tokens = print_tokens tokens; flush stdout

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
  print_token_list tokens; 
  let expr = Parser.parse tokens in 
  if !hadError then () else 
  print_endline (format_expr expr)
  

(** Runs the contents of the file found in [path]. If the code throws an error, 
    exit with code 65.*)
let runFile path =
  let file_channel = open_in path in 
  let code = In_channel.input_all file_channel in 
    run code; if !hadError then exit 65 else ()

(** Run an interactive command-line prompt to interpret code. If any inputted 
    line of code throws an error, report it but allow the user to continue
    inputting.  *)
let rec runPrompt () = 
  print_string "> "; flush stdout; 
  let line = input_line stdin in 
  if line = "" then () else (run line; hadError := false; runPrompt ())

