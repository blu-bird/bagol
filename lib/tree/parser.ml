open Token 
open Ast
open Errorhandling

module Parser = struct 

  let parseError token msg = 
    error_token token msg; ParseError

  (** [match_next_cond] returns the next instance 
      Precondition: [tokenList] and [tokenTypeList] are the same length. *)
  let match_next_cond tokenList tokenCond  = 
    match tokenList with 
    | tok :: tail when tokenCond tok.tokenType -> Some tok , tail 
    | _ -> None , tokenList 

  let consume tokenList tokenCond errMsg = 
    match tokenList with 
    | tok :: tail when tokenCond tok.tokenType -> tok, tail 
    | tok :: _ -> raise (parseError tok errMsg)
    | _ -> raise (Failure errMsg)
  
  (**[parseBinLoop tl tc exp op fst] parses the second argument to a 
     binary operator, forms a binary expression with [op] and [fst],
     and continues matching successive operations and arguments, if they exist. *)
  let rec parseBinLoop tokenList tokenCond exprParser op first_arg = 
    let second_arg , tail = exprParser tokenList in
    let next_tok , tail' = match_next_cond tail tokenCond in 
    let left_expr = 
      EBinary (op , first_arg , second_arg) in
    match next_tok with 
    | None -> left_expr , tail'
    | Some op' -> parseBinLoop tail' tokenCond exprParser op' left_expr

    (**[parseBinary tl tc exp] parses a binary expression from [tl] 
    whose terms are parsed with [exp] and whose operators satisfy [tc].*)
  let parseBinary tokenList tokenCond exprParser = 
    let first_arg , tail = exprParser tokenList in 
    let op , tail' = match_next_cond tail tokenCond in 
    match op with 
    | None -> first_arg, tail' 
    | Some tok -> parseBinLoop tail' tokenCond exprParser tok first_arg

  (**[parseUnary tl tc exp] parses a unary expression from [tl] where the operation
      satisfies [tc] and whose terms are parsed with [exp]. *)
  let rec parseUnary tokenList tokenCond exprParser = 
    let op , tail = match_next_cond tokenList tokenCond in 
    match op with 
    | None -> exprParser tokenList 
    | Some tok -> 
      let arg, tail' = parseUnary tail tokenCond exprParser in
      EUnary (tok , arg) , tail'

  let rec expression tokenList = assignment tokenList 
  
  and assignment tokenList = 
    let expr, tail = equality tokenList in 
    let eq_match, tail' = match_next_cond tail (function EQUAL -> true | _ -> false) in 
    match eq_match with 
    | None -> expr , tail'
    | Some eqtok -> let value, tail'' = assignment tail' in 
      match expr with 
      | EVar t -> EAssign (t, value) , tail''
      | _ -> raise (parseError eqtok "Invalid assignment target.")
  
  and equality tokenList = 
    parseBinary tokenList 
      (function | EQUAL_EQUAL | BANG_EQUAL -> true | _ -> false) comparison 

  and comparison tokenList = 
    parseBinary tokenList 
      (function | GREATER | GREATER_EQUAL | LESS | LESS_EQUAL -> true | _ -> false) term 

  and term tokenList = 
    parseBinary tokenList (function | MINUS | PLUS -> true | _ -> false) factor
  
  and factor tokenList = 
    parseBinary tokenList (function | SLASH | STAR -> true | _ -> false) unary
  
  and unary tokenList = 
    parseUnary tokenList (function | BANG | MINUS -> true | _ -> false ) primary
  
  and primary tokenList = 
    let token, tail = match_next_cond tokenList 
      (function | FALSE | TRUE | NIL | NUMBER | STRING | LEFT_PAREN | IDENTIFIER -> true | _ -> false) in 
    match token with 
    | Some tok -> 
      (match tok.tokenType with 
      | FALSE -> EBool false , tail
      | TRUE -> EBool true , tail 
      | NIL -> ENil , tail 
      | NUMBER -> (match tok.literal with 
        | Number f -> ENum f , tail | _ -> raise (Failure "Lexing error, not a number literal."))
      | STRING -> (match tok.literal with 
        | String s -> EStr s , tail | _ -> raise (Failure "Lexing error, not a string literal."))
      | LEFT_PAREN -> 
        let expr , tail' = expression tail in
        EGroup expr, snd (consume tail' (function | RIGHT_PAREN -> true | _ -> false) 
          "Expect ')' after expression.")
      | IDENTIFIER -> EVar tok, tail 
      | _ -> raise (parseError tok "Expect expression.")) 
    | None -> raise (Failure "Unreachable code, should be some primary.")
  
  let rec synchronizeLoop tokenList prev = 
    match tokenList with 
    | [] -> []
    | tok :: tail -> 
      if (tok.tokenType = EOF) then tokenList (* reached the end *)
      else if (prev.tokenType = SEMICOLON) then tokenList (* just saw semicolon, tok begins new statement *)
      else match tok.tokenType with 
      | CLASS | FOR | FUN | IF | PRINT | RETURN | VAR | WHILE -> tokenList 
      | _ -> synchronizeLoop tail tok 

  let synchronize tokenList = 
    let tok, tail = match_next_cond tokenList (fun _ -> true) in
    synchronizeLoop tail (match tok with Some t -> t | None -> failwith "Unreachable.")

  let printStatement tokenList = 
    let expr, tail = expression tokenList in 
    let tail' = snd (consume tail (function | SEMICOLON -> true | _ -> false) "Expect ';' after value.") in 
    Some (SPrint expr) , tail'
  
  let exprStatement tokenList = 
    let expr, tail = expression tokenList in 
    let tail' = snd (consume tail (function SEMICOLON -> true | _ -> false) "Expect ';' after value.") in 
    Some (SExpr expr) , tail'

  let rec statement tokenList = 
    let tok_opt, tail = match_next_cond tokenList (function PRINT | LEFT_BRACE -> true | _ -> false) in 
    match tok_opt with 
    | None -> exprStatement tail
    | Some tok -> match tok.tokenType with 
      | PRINT -> printStatement tail 
      | LEFT_BRACE -> block tail 
      | _ -> failwith "Unimplemented."

  and varDeclaration tokenList = 
    let tok, tail = consume tokenList (function IDENTIFIER -> true | _ -> false) 
      "Expect variable name." in 
    let assign , tail' = match_next_cond tail (function EQUAL -> true | _ -> false) in 
    match assign with 
    | None -> let _, tail'' = consume tail' (function SEMICOLON -> true | _ -> false) 
      "Expect ';' after variable declaration." in 
      Some (SVarDecl (tok, None)), tail''
    | Some _ -> 
      let expr, tail'' = expression tail' in 
      let _, tail''' = consume tail''  (function SEMICOLON -> true | _ -> false) 
      "Expect ';' after variable declaration." in 
      Some (SVarDecl (tok, Some expr)), tail'''

  and declaration tokenList = 
    try (
      let varTok, tail = match_next_cond tokenList (function VAR -> true | _ -> false) in 
      match varTok with 
      | None -> statement tail 
      | Some _ -> varDeclaration tail 
    ) 
    with 
    | ParseError -> None, synchronize tokenList 
  
  and blockLoop tokenList acc = 
    let nextTok, tail = match_next_cond tokenList (function EOF | RIGHT_BRACE -> false | _ -> true) in 
    match nextTok with  
    | Some _ -> let stmt_opt, tail' = declaration tokenList in 
      blockLoop tail' (stmt_opt :: acc)
    | None -> List.rev acc, tail 
  
  and block tokenList = 
    let stmtOptList, tail = blockLoop tokenList [] in 
    let _, tail' = consume tail (function RIGHT_BRACE -> true | _ -> false) "Expect '}' after block." in
    let stmtList = List.filter_map (fun x -> x) stmtOptList in
    Some (SBlock stmtList), tail'

  let rec parseStmtLoop tokenList acc = 
    let endTok, tail = match_next_cond tokenList (function | EOF -> true | _ -> false) in 
    match endTok with 
    | None -> let stmt, tail' = declaration tail in 
      parseStmtLoop tail' (stmt :: acc)
    | Some _ -> List.rev acc

  let parse tokenList = 
    (* EXPRESSIONS: *)
    (* try fst (expression tokenList) with 
    | ParseError -> ENil
    | _ -> raise (Failure "Not implemented.") *)
    (* STATEMENTS:  *)
    List.filter_map (fun x -> x) (parseStmtLoop tokenList [])

end 