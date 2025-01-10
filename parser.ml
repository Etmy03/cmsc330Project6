open LccTypes 

let match_token (toks : 'a list) (tok : 'a) : 'a list =
  match toks with
  | [] -> raise (Failure("List was empty"))
  | h::t when h = tok -> t
  | h::_ -> raise (Failure( 
      Printf.sprintf "Token passed in does not match first token in list"
    ))

let lookahead toks = match toks with
   h::t -> h
  | _ -> raise (Failure("Empty input to lookahead"))


(* Write your code below *)

let parse_lambda toks = 
  let rec parse_expr toks2 =
    match toks2 with
    | [] -> raise (Failure "List was empty")
    | Lambda_Var x :: t -> Var x, t
    | Lambda_LParen :: Lambda_Lambda :: Lambda_Var x :: Lambda_Dot :: rest ->
        let body, t = parse_expr rest in
        let func = Func (x, body) in
        let t' = match_token t Lambda_RParen in
        func, t'
    | Lambda_LParen :: rest ->
        let func, t = parse_expr rest in
        let arg, t' = parse_expr t in
        let app = Application (func, arg) in
        let t'' = match_token t' Lambda_RParen in
        app, t''
    | _ -> raise (Failure "Empty input to lookahead")

  in

  match toks with
  | [] -> raise (Failure "List was empty")
  | _ ->
      let ast, remaining_toks = parse_expr toks in
      if remaining_toks = [Lambda_EOF] then
        ast
      else
        raise (Failure "Parsing failed")
  

;;


let parse_engl toks = 
  let rec parse_C toks =
    match toks with
    | Engl_If :: rest ->
        let cond, rest' = parse_C rest in
        let then_branch, rest'' = match_token rest' Engl_Then |> parse_C in
        let else_branch, rest''' = match_token rest'' Engl_Else |> parse_C in
        If (cond, then_branch, else_branch), rest'''
    | _ -> parse_H toks

  and parse_H toks =
    let left, rest = parse_U toks in
    match lookahead rest with
    | Engl_And ->
        let op = lookahead rest in
        let right, rest' = match_token rest op |> parse_H in
        And (left, right), rest'
    | Engl_Or ->
        let op = lookahead rest in
        let right, rest' = match_token rest op |> parse_H in
        Or (left, right), rest'
    | _ -> left, rest

  and parse_U toks =
    match toks with
    | Engl_Not :: rest ->
        let operand, rest' = parse_U rest in
        Not operand, rest'
    | _ -> parse_M toks

  and parse_M toks =
    match toks with
    | Engl_True :: rest -> Bool true, rest
    | Engl_False :: rest -> Bool false, rest
    | Engl_LParen :: rest ->
        let expr, rest' = parse_C rest in
        let rest'' = match_token rest' Engl_RParen in
        expr, rest''
    | _ -> raise (Failure "Empty input to lookahead")
  in

  match toks with
  | [] -> raise (Failure "List was empty")
  | _ ->
      let ast, remaining_toks = parse_C toks in
      if remaining_toks = [Engl_EOF] then
        ast
      else
        raise (Failure "Parsing failed")
;;

