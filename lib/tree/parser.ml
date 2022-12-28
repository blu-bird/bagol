open Token 
open Ast
open Errorhandling

module Parser = struct 

  exception ParseError 

  let parseError token msg = 
    error_token token msg; ParseError

  let binop_of_tokenType = function 
  | BANG_EQUAL -> BBangEqual
  | EQUAL_EQUAL -> BEqualEqual
  | GREATER -> BGreater
  | GREATER_EQUAL -> BGreaterEqual
  | LESS -> BLess
  | LESS_EQUAL -> BLessEqual
  | MINUS -> BMinus
  | PLUS -> BPlus
  | SLASH -> BSlash
  | STAR -> BStar
  | _ -> raise (Failure "Not a valid binary operation.")

  let unop_of_tokenType = function 
  | MINUS -> UMinus
  | BANG -> UBang
  | _ -> raise (Failure "Not a valid unary operation.")

  (** [match_next_cond] returns the next instance 
      Precondition: [tokenList] and [tokenTypeList] are the same length. *)
  let match_next_cond tokenList tokenCond  = 
    match tokenList with 
    | tok :: tail when tokenCond tok.tokenType -> Some tok , tail 
    | _ -> None , tokenList 

  let consume tokenList tokenCond errMsg = 
    match tokenList with 
    | tok :: tail when tokenCond tok.tokenType -> tail 
    | tok :: _ -> raise (parseError tok errMsg)
    | _ -> raise (Failure errMsg)
  
  (**[parseBinLoop tl tc exp op fst] parses the second argument to a 
     binary operator, forms a binary expression with [op] and [fst],
     and continues matching successive operations and arguments, if they exist. *)
  let rec parseBinLoop tokenList tokenCond exprParser op first_arg = 
    let second_arg , tail = exprParser tokenList in
    let next_tok , tail' = match_next_cond tail tokenCond in 
    let left_expr = 
      EBinary (binop_of_tokenType (op.tokenType) , first_arg , second_arg) in
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
      EUnary (unop_of_tokenType tok.tokenType , arg) , tail'

  let rec expression tokenList = 
    equality tokenList 
  
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
      (function | FALSE | TRUE | NIL | NUMBER | STRING | LEFT_PAREN -> true | _ -> false) in 
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
        EGroup expr, consume tail' (function | RIGHT_PAREN -> true | _ -> false) 
          "Expect ')' after expression."
      | _ -> raise (parseError tok "Expect expression.")) 
    | None -> raise (Failure "Unreachable code, should be some primary.")


  let parse tokenList = 
    try fst (expression tokenList) with 
    | ParseError -> ENil
    | _ -> raise (Failure "Not implemented.")
end 