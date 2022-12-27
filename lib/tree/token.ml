  (** [tokenType] is a variant type capturing all the types of token that can 
      appear in Bagol. These are generated from characters that produce unique tokens, 
      characters that can produce either one-two character tokens, arbitrary-length
      tokens, or keywords (and the EOF token). *)
  type tokenType = 
  (* single char tokens *)
    LEFT_PAREN | RIGHT_PAREN | LEFT_BRACE | RIGHT_BRACE | COMMA | DOT | MINUS
  | PLUS | SEMICOLON | SLASH | STAR 
  (* one or two char tokens *)
  | BANG | BANG_EQUAL | EQUAL | EQUAL_EQUAL | GREATER | GREATER_EQUAL | LESS 
  | LESS_EQUAL 
  (* literals *)
  | IDENTIFIER | STRING | NUMBER 
  (* keywords *)
  | AND | CLASS | ELSE | FALSE | FUN | FOR | IF | NIL | OR | PRINT | RETURN 
  | SUPER | THIS | TRUE | VAR | WHILE 
  | EOF 

  (**[literal] is a type capturing all the different literals that can appear 
in Bagol tokens, but only for numbers or strings. Tokens otherwise have the Null 
literal assigned to them. *)
  type literal = 
  | Number of float
  | String of string 
  | Null 
 
  (**[token]s have a tokenType, a corresponding [lexeme] consisting of the characters
      corresponding to the token, the [literal] semantic meaning of the token, and
      what line number [line] it appeared on.*)
  type token = 
    {tokenType : tokenType; lexeme : string; literal : literal; line : int}

  (** [string_of_tokenType] generates the string representation of every term
      of type [tokenType]. Used for unit testing. *)
    let string_of_tokenType = function 
   (* single char tokens *)
   | LEFT_PAREN -> "LEFT_PAREN"
   | RIGHT_PAREN -> "RIGHT_PAREN"
   | LEFT_BRACE -> "LEFT_BRACE" 
   | RIGHT_BRACE -> "RIGHT_BRACE"
   | COMMA -> "COMMA" 
   | DOT -> "DOT"
   | MINUS -> "MINUS"
   | PLUS -> "PLUS"
   | SEMICOLON -> "SEMICOLON" 
   | SLASH -> "SLASH"
   | STAR -> "STAR" 
   (* one or two char tokens *)
   | BANG -> "BANG"
   | BANG_EQUAL -> "BANG_EQUAL"
   | EQUAL -> "EQUAL"
   | EQUAL_EQUAL -> "EQUAL_EQUAL"
   | GREATER -> "GREATER"
   | GREATER_EQUAL -> "GREATER_EQUAL"
   | LESS -> "LESS"
   | LESS_EQUAL -> "LESS_EQUAL"
   (* literals *)
   | IDENTIFIER -> "IDENTIFIER"
   | STRING -> "STRING"
   | NUMBER -> "NUMBER"
   (* keywords *)
   | AND -> "AND"
   | CLASS -> "CLASS"
   | ELSE -> "ELSE"
   | FALSE -> "FALSE"
   | FUN -> "FUN"
   | FOR -> "FOR"
   | IF -> "IF"
   | NIL -> "NIL"
   | OR -> "OR"
   | PRINT -> "PRINT"
   | RETURN -> "RETURN"
   | SUPER -> "SUPER"
   | THIS -> "THIS"
   | TRUE -> "TRUE"
   | VAR -> "VAR"
   | WHILE -> "WHILE"
   | EOF -> "EOF"

   (**[string_of_literal] convertes the literal data associated with a token into
       a string. Used for unit testing. *)
  let string_of_literal = function 
  | Number f -> string_of_float f 
  | String s -> s
  | Null -> "null"

  (**[string_of_token t] prints the token [t]'s data in the form "tokenType lexeme literal".*)
  let string_of_token t = 
    Printf.sprintf "%s %s %s" (string_of_tokenType t.tokenType) t.lexeme (string_of_literal t.literal)
