  
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

  
