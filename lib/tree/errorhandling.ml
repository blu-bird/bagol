
  (** Boolean flag for whether the code to be interpreted has errored or not. *)
let hadError = ref false 

(** prints error report on line [line] with message [message]. *)
let report line where message = 
  print_endline (Printf.sprintf "[line %d] Error%s: %s" line where message); 
  hadError := true

(** prints *)
let error line msg = report line "" msg

