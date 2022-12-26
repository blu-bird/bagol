  
  type token_t = 
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

  type literal = 
  | Number of float
  | String of string 
  | Null 
 
  type token = 
    {tok_type : token_t; lexeme : string; literal : literal; line : int}

  let string_of_token_t = function 
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

  let string_of_literal = function 
  | Number f -> string_of_float f 
  | String s -> s
  | Null -> "null"

  let string_of_token t = 
    Printf.sprintf "%s %s %s" (string_of_token_t t.tok_type) t.lexeme (string_of_literal t.literal)
