open Token
open Errorhandling

module Scanner = struct 
  (** Representation type for Scanner. Scanner operates on a sequence of characters converted from a string (since there's a built-in function to map [string] -> [char Seq.t].)*)
  type t = char Seq.t

  (** [line] points to the current line number being processed. *)
  let line = ref 1 

  (** [single c] returns the correct [tokenType] for a character given it is the only character representing that token.
      Precondition: the character [c] must be the only character in the generated token.
      Raises an Failure if called on a character not representing a token in Bagol. *)
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
    | _ -> raise (Failure "Unreachble code.") (* should be unreachable *)

  (** [make_token tt lex lit] creates a new token with [tokenType] [tt], lexeme [lex],
      literal [lit], on the line currently pointed to by the ref [line]. *)
  let make_token tt lex lit = 
    {tokenType = tt; lexeme = lex; literal = lit; line = !line}

  (** [match_next expected seq] checks if the next token in the character sequence
      [seq] matches [expected], and returns a pair with the boolean value of this
  check and the character consumed in the sequence if there is a match. *)  
  let match_next expected seq = 
     match seq () with 
     | Seq.Nil -> false, Seq.empty
     | Seq.Cons (h, st) -> 
        let hit = h = expected in 
        hit, (if hit then st else seq)

  (** [double c d] returns the correct two-character token with characters
      [c] and [d].  *)
  let double = function 
  | '!' -> fun _ -> BANG_EQUAL  
  | '=' -> fun _ -> EQUAL_EQUAL  
  | '<' -> fun _ -> LESS_EQUAL  
  | '>' -> fun _ -> GREATER_EQUAL  
  | _ -> fun _ -> raise (Failure "Unreachable code.")

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
      Some (make_token STRING ((String.make 1 '"') ^ text ^ (String.make 1 '"')) (String text)), ts')

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
      Some (make_token NUMBER full_num (Number (Float.of_string full_num))) , ts' 
    else Some (make_token NUMBER text (Number (Float.of_string text))), trim_seq) 

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
    Some (make_token (keywords text) text Null), trim_seq

  let scanToken h st = 
    match h with
    | '(' | ')' | '{' | '}' | ',' | '.' | '-' | '+' | ';' | '*'  -> 
      Some (make_token (single h) (String.make 1 h) Null), st
    | '!' | '=' | '<' | '>' -> (* assumes all one/two tokens have second token '=' *)
      let matchFound, st' = match_next '=' st in 
      (if matchFound then 
        Some (make_token (double h '=') (String.make 1 h ^ "=") Null)
      else 
        Some (make_token (single h) (String.make 1 h) Null)) , st'
    | '/' -> 
      let match_found , st' = match_next '/' st in 
      (if match_found then None, Seq.drop_while (fun c -> c <> '\n') st'
      else Some (make_token (single h) (String.make 1 h) Null) , st) 
    | ' ' | '\r' | '\t' -> None, st
    | '\n' -> incr line; None, st
    | '"' -> next_string st
    | _ -> 
      (if isDigit(h) then next_num (Seq.cons h st) else 
      (if isAlpha(h) then next_identifier (Seq. cons h st)
        else (error !line "Unexpected character."; None , st)))

  let rec scanTokens_opt t = 
    match t () with 
    | Seq.Nil -> [Some (make_token EOF "" Null)] 
    | Seq.Cons (h, st) -> let token , tail = scanToken h st in 
      token :: scanTokens_opt tail
  
  let scanTokens t = t |> scanTokens_opt |> List.filter_map (fun x -> x)
end 