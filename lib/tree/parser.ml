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

  let check tType = (fun tt -> if tt = tType then true else false)

  let checks tTypes = 
    List.fold_left (fun f t tt -> check t tt || f tt) (fun _ -> false) tTypes 
    
  let ( >>= ) (opt, st) (f, g) =
    match opt with 
    | None -> f st
    | Some v -> g (v, st)

  let ( ><= ) (v, st) (f, g) = (f v, g st)

  (**[parseBinLoop tl tc exp op fst] parses the second argument to a 
     binary operator, forms a binary expression with [op] and [fst],
     and continues matching successive operations and arguments, if they exist. *)
  let rec parseBinLoop tokenList tokenCond exprParser op first_arg = 
    let second_arg , tail = exprParser tokenList in
    let left_expr = match op.tokenType with 
      | AND | OR -> ELogic (op, first_arg , second_arg)
      | _ -> EBinary (op , first_arg , second_arg) in
    match_next_cond tail tokenCond >>=
      ((fun tl -> (left_expr, tl)) ,
      (fun (op', tl) -> parseBinLoop tl tokenCond exprParser op' left_expr))

    (**[parseBinary tl tc exp] parses a binary expression from [tl] 
    whose terms are parsed with [exp] and whose operators satisfy [tc].*)
  let parseBinary tokenList tokenCond exprParser = 
    let first_arg , tail = exprParser tokenList in 
    match_next_cond tail tokenCond >>=
      ((fun tl -> first_arg, tl), 
      (fun (t, tl) -> parseBinLoop tl tokenCond exprParser t first_arg))

  (**[parseUnary tl tc exp] parses a unary expression from [tl] where the operation
      satisfies [tc] and whose terms are parsed with [exp]. *)
  let rec parseUnary tokenList tokenCond exprParser = 
    match_next_cond tokenList tokenCond >>=
    ((fun _ -> exprParser tokenList), 
    (fun (t, tl) -> parseUnary tl tokenCond exprParser ><= 
      ((fun arg -> EUnary (t , arg)) , fun tl -> tl)))

  let rec expression tokenList = assignment tokenList 
  
  and assignment tokenList = 
    let expr, tail = logic_or tokenList in 
    match_next_cond tail (check EQUAL) >>=
      ((fun tl -> expr, tl), 
      (fun (t, tl) -> let value, tl' = assignment tl in 
        match expr with 
        | EVar t -> EAssign (t, value) , tl'
        | _ -> raise (parseError t "Invalid assignment target.")) )
  
  and logic_or tokenList = 
      parseBinary tokenList (check OR) logic_and
  
  and logic_and tokenList = 
      parseBinary tokenList (check AND) equality 

  and equality tokenList = 
    parseBinary tokenList (checks [EQUAL_EQUAL; BANG_EQUAL]) comparison 

  and comparison tokenList = 
    parseBinary tokenList (checks [GREATER; GREATER_EQUAL; LESS; LESS_EQUAL]) term 

  and term tokenList = 
    parseBinary tokenList (checks [MINUS; PLUS]) factor
  
  and factor tokenList = 
    parseBinary tokenList (checks [SLASH; STAR]) unary
  
  and unary tokenList = 
    parseUnary tokenList (checks [BANG; MINUS]) call

  and callLoop tokenList expr = 
    match_next_cond tokenList (check LEFT_PAREN) >>=
    ((fun tl -> expr, tl), 
    (fun (_, tl) -> let call_expr, finishTail = finishCall expr tl in 
    callLoop finishTail call_expr ))

  and call tokenList = 
    let expr, tail = primary tokenList in callLoop tail expr 

  and finishCallLoop tokenList args = 
    if List.length args >= 255 then 
      error_token (List.hd tokenList) "Can't have more than 255 arguments." else (); 
    let expr, exprTail = expression tokenList in
    let next_args = expr :: args in 
    match_next_cond exprTail (check COMMA) >>=
      ((fun tl -> next_args, tl), 
      (fun (_, tl) -> finishCallLoop tl next_args))
      
  and finishCall e tokenList = 
    let arguments, pTail = 
      (match_next_cond tokenList (check RIGHT_PAREN) >>=
        ((fun _ -> finishCallLoop tokenList []), (fun _ -> [], tokenList))) in 
    let rp, tail = consume pTail (check RIGHT_PAREN) "Expect ')' after arguments" in 
    ECall (e, rp, List.rev arguments), tail

  and primary tokenList = 
    match_next_cond tokenList (checks [FALSE; TRUE; NIL; NUMBER; STRING; LEFT_PAREN; IDENTIFIER]) >>=
    ((fun tl -> raise (parseError (List.hd tl) "Expect expression.")
      (* failwith ("Unreachable code, primary expected on " ^ string_of_token (List.hd tl)) *)
      ), 
    (fun (t, tl) -> match t.tokenType with 
    | FALSE -> EBool false , tl
    | TRUE -> EBool true , tl 
    | NIL -> ENil , tl 
    | NUMBER -> (match t.literal with 
      | Number f -> ENum f , tl | _ -> raise (Failure "Lexing error, not a number literal."))
    | STRING -> (match t.literal with 
      | String s -> EStr s , tl | _ -> raise (Failure "Lexing error, not a string literal."))
    | LEFT_PAREN -> 
      let expr , tail' = expression tl in
      EGroup expr, snd (consume tail' (check RIGHT_PAREN) 
        "Expect ')' after expression.")
    | IDENTIFIER -> EVar t, tl 
    | _ -> raise (parseError t "Expect expression.")))
  
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
    let tail' = snd (consume tail (check SEMICOLON) "Expect ';' after value.") in 
    Some (SPrint expr) , tail'
  
  let exprStatement tokenList = 
    let expr, tail = expression tokenList in 
    let tail' = snd (consume tail (check SEMICOLON) "Expect ';' after value.") in 
    Some (SExpr expr) , tail'
  
  let rec statement tokenList = 
    match_next_cond tokenList (checks [PRINT; LEFT_BRACE; IF; WHILE; FOR; RETURN]) >>=
    ((fun tl -> exprStatement tl),
    (fun (t, tl) -> match t.tokenType with 
      | PRINT -> printStatement tl
      | LEFT_BRACE -> blockStatement tl
      | IF -> ifStatement tl 
      | WHILE -> whileStatement tl 
      | FOR -> forStatement tl 
      | RETURN -> returnStatement t tl 
      | _ -> failwith "Unimplemented."))

  and varDeclaration tokenList = 
    let tok, tail = consume tokenList (check IDENTIFIER) 
      "Expect variable name." in 
    let assign , tail' = match_next_cond tail (check EQUAL) in 
    match assign with 
    | None -> let _, tail'' = consume tail' (check SEMICOLON) 
      "Expect ';' after variable declaration." in 
      Some (SVarDecl (tok, None)), tail''
    | Some _ -> 
      let expr, tail'' = expression tail' in 
      let _, tail''' = consume tail'' (check SEMICOLON) 
      "Expect ';' after variable declaration." in 
      Some (SVarDecl (tok, Some expr)), tail'''

  and declaration tokenList = 
    try (
      match_next_cond tokenList (checks [VAR; FUN]) >>=
      ((fun tl -> statement tl),
      (fun (t, tl) -> match t.tokenType with 
        | VAR -> varDeclaration tl 
        | FUN -> funcDeclaration "function" tl
        | _ -> failwith "Unimplemented." ))
    ) 
    with 
    | ParseError -> None, synchronize tokenList 
  
  and blockLoop tokenList acc = 
    match_next_cond tokenList (fun t -> not (checks [EOF; RIGHT_BRACE] t)) >>=
      ((fun tl -> List.rev acc, tl), 
      (fun _ -> let stmt_opt, tail' = declaration tokenList in 
      blockLoop tail' (stmt_opt :: acc)))

  and blockStatement tokenList = 
    let stmtOptList, tail = blockLoop tokenList [] in 
    let _, tail' = consume tail (check RIGHT_BRACE) "Expect '}' after block." in
    let stmtList = List.filter_map (fun x -> x) stmtOptList in
    Some (SBlock stmtList), tail'

  and ifStatement tokenList = 
    let _, tail = consume tokenList (check LEFT_PAREN)
      "Expect '(' after 'if'." in 
    let guard, guardTail = expression tail in 
    let rparen, tail' = consume guardTail (check RIGHT_PAREN)
      "Expect ')' after if condition." in 
    statement tail' >>=
      ((fun _ -> raise (parseError rparen "Expect statement after 'if'.")), 
      (fun (thenStmt, thenTl) -> match_next_cond thenTl (check ELSE) >>=
        ((fun elseTl -> Some (SIf (guard, thenStmt, None)), elseTl), 
        (fun (elseTok, tl) -> statement tl >>=
          ((fun _ -> raise (parseError elseTok "Expect statement after 'else'.")), 
          (fun (elseStmt, elseTl) -> Some (SIf (guard, thenStmt, Some elseStmt)), elseTl))))))

  and whileStatement tokenList = 
    let _, tail = consume tokenList (check LEFT_PAREN)
    "Expect '(' after 'while'." in 
  let cond, guardTail = expression tail in 
  let rparen, tail' = consume guardTail (check RIGHT_PAREN)
    "Expect ')' after condition." in 
  statement tail' >>=
    ((fun _ -> raise (parseError rparen "Expect statement after 'while'.")),
    (fun (body, whileTail) -> Some (SWhile (cond, body)), whileTail ))

  and forStatement tokenList = 
    let _, guardTail = consume tokenList (check LEFT_PAREN)
      "Expect '(' after 'for'." in 
    let initOpt, initTail = match_next_cond guardTail (checks [SEMICOLON; VAR]) >>= 
      ((fun tl -> exprStatement tl), 
      (fun (t, tl) -> match t.tokenType with 
        | SEMICOLON -> None, tl | VAR -> varDeclaration tl
        | _ -> failwith "Invalid token following '('.")) in 
    let condOpt, condTail = match_next_cond initTail (check SEMICOLON) >>= 
      ((fun tl -> expression tl ><= ((fun e -> Some e), fun tl' -> snd (consume tl' (check SEMICOLON) "Expect ';' after loop condition."))), 
      (fun (_, tl) -> None, tl)) in 
    let incrOpt, incrTail = match_next_cond condTail (check RIGHT_PAREN) >>=
      ((fun tl -> expression tl ><= ((fun e -> Some e), fun tl' -> snd (consume tl' (check RIGHT_PAREN) "Expect ';' after loop condition."))), 
      (fun (_, tl) -> None, tl)) in 
    let sOpt, loopTail = statement incrTail in 
    let body = match sOpt with None -> failwith "For loop must have body." | Some s -> s in 
    let whileBody = match incrOpt with None -> body | Some incr -> 
      let copyExpr = copy_expr incr in 
      SBlock [body; SExpr copyExpr] in 
    let whileLoop = SWhile ((match condOpt with None -> EBool true | Some cond -> cond), whileBody) in
    let forBlock = match initOpt with None -> whileLoop | Some init -> SBlock [init; whileLoop] in 
    Some forBlock, loopTail

  and paramLoop tokenList params = 
  if List.length params >= 255 then 
    error_token (List.hd tokenList) "Can't have more than 255 parameters." else (); 
  let id, idTail = consume tokenList (check IDENTIFIER) "Expect parameter name." in 
  let next_params = id :: params in 
  match_next_cond idTail (check COMMA) >>=
    ((fun tl -> next_params, tl), (fun (_, tl) -> paramLoop tl next_params))

  and funcDeclaration kind tokenList = 
  let name, nameTail = consume tokenList (check IDENTIFIER) ("Expect " ^ kind ^ " name.") in 
  let _, parenTail = consume nameTail (check LEFT_PAREN) ("Expect '(' after " ^ kind ^ " name.") in 
  let params, paramTail = match_next_cond parenTail (check RIGHT_PAREN) >>=
    ((fun tl -> paramLoop tl []), (fun _ -> [], parenTail)) in 
  let _, rParenTail = consume paramTail (check RIGHT_PAREN) "Expect ')' after parameters." in 
  let lBrace, lBraceTail = consume rParenTail (check LEFT_BRACE) ("Expect '{' before " ^ kind ^ " body.") in 
  blockStatement lBraceTail >>=
    ((fun _ -> raise (parseError lBrace "Expecting statement after '{'.")), 
    (fun (stmt, bodyTail) -> 
      let stmtList = match stmt with | SBlock sl -> sl | _ -> raise (parseError lBrace "Expecting block statement after '{'.") in 
      (Some (SFun (name, params, stmtList))), bodyTail))

  and returnStatement tok tokenList = 
    match_next_cond tokenList (check SEMICOLON) >>=
    ((fun tl -> let expr, nextTail = expression tl in 
      let _, tail = consume nextTail (check SEMICOLON) "Expect ';' after return value." in
      Some (SReturn (tok, Some expr)), tail), 
    (fun (_, tl) -> Some (SReturn (tok, None)), tl)) 
   
  (* Some (SReturn (tok, exprOpt)), tail  *)
   
  let rec parseStmtLoop tokenList acc = 
    let endTok, tail = match_next_cond tokenList (check EOF) in 
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