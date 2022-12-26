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

  let strCharLst lst = lst |> List.map (String.make 1) |> String.concat ""

  let rec munch_group cond acc seq = 
    if cond (peek seq) then 
      match seq () with 
      | Seq.Nil -> (List.rev acc), Seq.empty
      | Seq.Cons (h, ts) -> (if h = '\n' then incr line else ()); 
        munch_group cond (h :: acc) ts
    else (List.rev acc), seq

  let next_string seq = 
    let cs, trim_seq = munch_group (fun c -> c <> '"') [] seq in 
    (match trim_seq () with 
    | Seq.Nil -> error !line "Unterminated string."; None, Seq.empty
    | Seq.Cons (_, ts') -> 
      let text = strCharLst cs in 
      Some {tok_type = STRING; lexeme = (String.make 1 '"') ^ text ^ (String.make 1 '"'); literal = String text; line = !line}, ts')

  let isDigit c = Char.compare c '0' >= 0 && Char.compare c '9' <= 0

  let peek_next seq = 
    match seq () with 
    | Seq.Nil -> '\x00'
    | Seq.Cons (_, ts) -> 
      (match ts () with | Seq.Nil -> '\x00' | Seq.Cons (h, _) -> h)

  let next_num seq = 
    let cs, trim_seq = munch_group (fun c -> isDigit c) [] seq in
    let text = strCharLst cs in 
    (if peek trim_seq = '.' && isDigit (peek_next trim_seq) then 
      let decimal, ts' = munch_group (fun c -> isDigit c) [] (Seq.drop 1 trim_seq) in 
      let full_num = text ^ "." ^ strCharLst decimal in
      Some {tok_type = NUMBER; lexeme = full_num; literal = Number (Float.of_string full_num) ; line = !line}, ts' 
    else Some {tok_type = NUMBER; lexeme = text; literal = Number (Float.of_string text); line = !line}, trim_seq)

  let isAlpha c = (Char.compare c 'a' >= 0 && Char.compare c 'z' <= 0) || 
    (Char.compare c 'A' >= 0 && Char.compare c 'Z' <= 0)  || c = '_'

  let keywords = function 
  | "and" -> AND
  | "class" -> CLASS
  | "else" -> ELSE
  | "false" -> FALSE
  | "for" -> FOR
  | "fun" -> FUN
  | "if" -> IF
  | "nil" -> NIL
  | "or" -> OR
  | "print" -> PRINT
  | "return" -> RETURN
  | "super" -> SUPER
  | "this" -> THIS
  | "true" -> TRUE
  | "var" -> VAR
  | "while" -> WHILE
  | _ -> IDENTIFIER

  let next_identifier seq = 
    let cs, trim_seq = munch_group (fun c -> isAlpha c || isDigit c) [] seq in 
    let text = strCharLst cs in  
    Some {tok_type = keywords text; lexeme = text; literal = Null; line = !line}, trim_seq


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
      (if match_found then None, Seq.drop_while (fun c -> c <> '\n') st'
      else Some {tok_type = single h; lexeme = String.make 1 h; literal = Null; line = !line}, st)
    | ' ' | '\r' | '\t' -> None, st
    | '\n' -> incr line; None, st
    | '"' -> next_string st
    | _ -> 
      (if isDigit(h) then next_num (Seq.cons h st) else 
      (if isAlpha(h) then next_identifier (Seq. cons h st)
        else (error !line "Unexpected character."; None , st)))

  let rec scanTokens_opt t = 
    match t () with 
    | Seq.Nil -> [Some {tok_type = EOF; lexeme = ""; literal = Null; line = !line}]
    | Seq.Cons (h, st) -> let token , tail = scanToken h st in 
      token :: scanTokens_opt tail
  
  let scanTokens t = t |> scanTokens_opt |> List.filter_map (fun x -> x)
end 