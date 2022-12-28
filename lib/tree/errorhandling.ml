open Token 

(** [hadError] is a flag for whether the code to be interpreted has errored or not. *)
let hadError = ref false 

let report line where message = 
  print_endline (Printf.sprintf "[line %d] Error%s: %s" line where message); 
  hadError := true

let error line msg = report line "" msg

(**[error_token] reports a parsing error on [token] with [msg]. *)
let error_token token msg = 
  if token.tokenType == EOF then 
    report token.line " at end" msg 
  else 
    report token.line (" at '" ^ token.lexeme ^ "'") msg 


