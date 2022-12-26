open Scanner
open Token
open Errorhandling


let rec print_tokens tokens = 
  match tokens with 
  | [] -> ()
  | h :: [] -> print_string h.lexeme
  | h :: h' :: t -> print_string h.lexeme; print_string ", "; print_tokens (h' :: t)

let print_token_list tokens = 
  print_string "["; print_tokens tokens; print_endline "]"; flush stdout

let run src = print_endline src; 
  let tokens = Scanner.scanTokens (String.to_seq src) in 
  print_token_list tokens
  

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

