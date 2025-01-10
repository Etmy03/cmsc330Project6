open LccTypes

let is_valid_char c =
  let validStr = Str.regexp "[a-z.()L]" in
  Str.string_match validStr (String.make 1 c) 0
;;

let rec reverse_list lst =
  let rec reverse_acc acc = function
    | [] -> acc
    | x :: xs -> reverse_acc (x :: acc) xs
  in
  reverse_acc [] lst
;;

let rec string_to_list str =
  match str with
  | "" -> []
  | s -> s.[0] :: string_to_list (String.sub s 1 (String.length s - 1))
;;

let newStr input =
  let rec newS lst acc = match lst with
    | '(' :: rest -> newS rest ("(" :: acc)
    | ')' :: rest -> newS rest (")" :: acc)
    | 't' :: 'r' :: 'u' :: 'e' :: rest -> newS rest ("true" :: acc)
    | 'f' :: 'a' :: 'l' :: 's' :: 'e' :: rest -> newS rest ("false" :: acc)
    | 'i' :: 'f' :: rest -> newS rest ("if" :: acc)
    | 't' :: 'h' :: 'e' :: 'n' :: rest -> newS rest ("then" :: acc)
    | 'e' :: 'l' :: 's' :: 'e' :: rest -> newS rest ("else" :: acc)
    | 'a' :: 'n' :: 'd' :: rest -> newS rest ("and" :: acc)
    | 'o' :: 'r' :: rest -> newS rest ("or" :: acc)
    | 'n' :: 'o' :: 't' :: rest -> newS rest ("not" :: acc)
    | '\t' :: rest -> newS rest acc
    | '\n' :: rest -> newS rest acc
    | ' ' :: rest -> newS rest acc
    | c :: rest -> newS rest (String.make 1 c :: acc)
    | [] -> reverse_list acc
  in
  newS (string_to_list input) [] 
;;

let lex_lambda input = 
    let rec lex_lambda_helper str acc =
    match str with
    | [] -> reverse_list (Lambda_EOF :: acc) 
    | c :: rest -> if c = ' ' then lex_lambda_helper rest acc
        else
            if is_valid_char c then
            match c with
            | '(' -> lex_lambda_helper rest (Lambda_LParen :: acc)
            | ')' -> lex_lambda_helper rest (Lambda_RParen :: acc)
            | '.' -> lex_lambda_helper rest (Lambda_Dot :: acc)
            | 'L' -> lex_lambda_helper rest (Lambda_Lambda :: acc)
            | _   -> lex_lambda_helper rest (Lambda_Var (String.make 1 c) :: acc)
            else
            raise (Failure "tokenizing failed")
    in
    lex_lambda_helper (string_to_list input) []
;;


let lex_engl input = 
    let myList = newStr input in
    let rec lex_engl_helper lst acc =
        match lst with
        | [] -> reverse_list (Engl_EOF :: acc)
        | "" :: rest -> lex_engl_helper rest acc
        | "(" :: rest -> lex_engl_helper rest (Engl_LParen :: acc)
        | ")" :: rest -> lex_engl_helper rest (Engl_RParen :: acc)
        | "true" :: rest -> lex_engl_helper rest (Engl_True :: acc)
        | "false" :: rest -> lex_engl_helper rest (Engl_False :: acc)
        | "if" :: rest -> lex_engl_helper rest (Engl_If :: acc)
        | "then" :: rest -> lex_engl_helper rest (Engl_Then :: acc)
        | "else" :: rest -> lex_engl_helper rest (Engl_Else :: acc)
        | "not" :: rest -> lex_engl_helper rest (Engl_Not :: acc)
        | "and" :: rest -> lex_engl_helper rest (Engl_And :: acc)
        | "or" :: rest -> lex_engl_helper rest (Engl_Or :: acc)
        |_ ->raise (Failure "tokenizing failed")
    in lex_engl_helper myList []
;;
