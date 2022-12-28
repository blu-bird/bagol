type value = 
| VNil 
| VBool of bool 
| VNum of float 
| VStr of string 

let string_of_val = function 
| VNil -> "nil"
| VBool b -> string_of_bool b
| VNum f -> let sf = string_of_float f in 
  (if sf.[String.length sf - 1] = '.' then 
    String.sub sf 0 (String.length sf - 1)
  else sf)
| VStr s -> s 