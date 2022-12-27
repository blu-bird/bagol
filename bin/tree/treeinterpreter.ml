open Tree.Bagol

(** Main function that parses command-line arguments. *)
let () =
  print_endline ""; flush stdout; 
  if Array.length Sys.argv > 2 then 
    (print_endline "Usage: olox [script]"; exit 64)
  else if Array.length Sys.argv = 2 then 
    runFile(Sys.argv.(1))
  else 
    runPrompt ()

