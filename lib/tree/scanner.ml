open Token
open Errorhandling

module Scanner = struct 
  type t = char Seq.t
  let line = ref 1 

  let single = function 
  (* strict single *)
    | '(' -> LEFT_PAREN
    | ')' -> RIGHT_PAREN
    | '{' -> LEFT_BRACE 
    | '}' -> RIGHT_BRACE 
    | ',' -> COMMA 
    | '.' -> DOT 
    | '-' -> MINUS
    | '+' -> PLUS 
    | ';' -> SEMICOLON 
    | '/' -> SLASH 
    | '*' -> STAR 
  (* could be part of double *)
    | '!' -> BANG
    | '=' -> EQUAL
    | '<' -> LESS
    | '>' -> GREATER
    | _ -> EOF (* should be unreachable *)
  
  let match_next expected seq = 
     match seq () with 
     | Seq.Nil -> false, Seq.empty
     | Seq.Cons (h, st) -> 
        let hit = h = expected in 
        hit, (if hit then st else seq)

  let double = function 
  | '!' -> fun _ -> BANG_EQUAL  
  | '=' -> fun _ -> EQUAL_EQUAL  
  | '<' -> fun _ -> LESS_EQUAL  
  | '>' -> fun _ -> GREATER_EQUAL  
  | _ -> fun _ -> EOF

  let peek seq = 
    match seq () with 
    | Seq.Nil -> '\x00'
    | Seq.Cons (h, _) -> h

  let rec ignore_until cond seq = 
    if cond (peek seq) then 
      match seq () with 
      | Seq.Nil -> Seq.empty
      | Seq.Cons (_, ts) -> ignore_until cond ts
    else seq

  let scanToken h st = 
    match h with
    | '(' | ')' | '{' | '}' | ',' | '.' | '-' | '+' | ';' | '*'  -> 
      Some {tok_type = single h; lexeme = String.make 1 h; literal = Null; line = !line}, st
    | '!' | '=' | '<' | '>' -> (* assumes all one/two tokens have second token '=' *)
      let matchFound, st' = match_next '=' st in 
      (if matchFound then 
        Some {tok_type = double h '='; lexeme = String.make 1 h ^ "="; literal = Null; line = !line}
      else 
        Some {tok_type = single h; lexeme = String.make 1 h; literal = Null; line = !line}) , st'
    | '/' -> 
      let match_found , st' = match_next '/' st in 
      (if match_found then 
        None, ignore_until (fun c -> c <> '\n') st'
      else Some {tok_type = single h; lexeme = String.make 1 h; literal = Null; line = !line}, st)
    | _ -> error !line "Unexpected token."; 
      None , st

  let rec scanTokens_opt t = 
    match t () with 
    | Seq.Nil -> [Some {tok_type = EOF; lexeme = ""; literal = Null; line = !line}]
    | Seq.Cons (h, st) -> let token , tail = scanToken h st in 
      token :: scanTokens_opt tail
  
  let scanTokens t = t |> scanTokens_opt |> List.filter_map (fun x -> x)
end 