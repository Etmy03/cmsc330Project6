open LccTypes 

let cntr = ref (-1)

let fresh () =
  cntr := !cntr + 1 ;
  !cntr

let rec lookup env var = match env with 
  [] -> None
  |(v,e)::t -> if v = var then e else lookup t var

let rec list_length lst count =
  match lst with
  | [] -> count
  | _ :: t -> list_length t (count + 1)
;;

let create_environment expr  =
  let rec gather_vars expr env =
    match expr with
    | Var x -> (x, None) :: env
    | Func (x, body) -> (x, Some (body)) :: env
    | Application (e1, e2) ->
        let env1 = gather_vars e1 env in
        gather_vars e2 env1
  in

  gather_vars expr []
;;

let rec compare_envs env1 env2 =
  list_length env1 0 = list_length env2 0
;;

let rec alpha_convert e = 
    let rec convert_helper expr env =
    match expr with
    | Var x -> Var (lookup env x |> function Some v -> v | None -> x)
    | Func (x, body) ->
        let new_var = fresh () |> string_of_int in
        let new_env = (x, Some new_var) :: env in
        Func (new_var, convert_helper body new_env)
    | Application (e1, e2) ->
        let converted_e1 = convert_helper e1 env in
        let converted_e2 = convert_helper e2 env in
        Application (converted_e1, converted_e2)
  in
  convert_helper e []
;;

let isalpha e1 e2 = 
  let rec isalpha_helper env1 env2 expr1 expr2 =
      match expr1, expr2 with
      | Var x1, Var x2 ->
          (match lookup env1 x1, lookup env2 x2 with
          | Some _, Some _ -> true
          | None, None -> x1 = x2
          | _, _ -> false)
      | Func (x1, Var y1), Func (x2, Var y2) -> 
          if ( ((x1 = y1) && (x2 = y2)) || ((x1 <> y1) && (x2 <> y2)) ) then true else false
      | Func(y1,Application(Var(y2),Var(y3))), Func(x1,Application(Var(x2),Var(x3))) -> 
          if ((((y1=y2) && (y2=y3)) && ((x1=x2) && (x2=x3))) || 
              (((y1=y2) && (y1<>y3)) && ((x1=x2) && (x1<>x3))) || 
              (((y1=y3) && (y1<>y2)) && ((x1=x3) && (x1<>x2))) || 
              (((y3=y2) && (y1<>y3)) && ((x3=x2) && (x1<>x3))) ||
              (((y3<>y2) && (y1<>y3)) && ((x3<>x2) && (x1<>x3)))
          ) then true else false
      | Application (Var(x1), Var(x2)), Application (Var(y1), Var(y2)) ->
            if ( ((x1 = x2) && (y1 = y2)) || ((x1 <> x2) && (y1 <> y2)) ) then true else false
      | Application (f1, a1), Application (f2, a2) -> (match f1, f2, a1, a2 with
                                                      | Func(_,_), Func(_,_), Var _, Var _ -> isalpha_helper env1 env2 f1 f2
                                                      | Application(_,_), Application(_,_), Var _, Var _ -> isalpha_helper env1 env2 f1 f2
                                                      | Var _, Var _, Application(_,_), Application(_,_) -> isalpha_helper env1 env2 a1 a2
                                                      | _, _, _, _ -> isalpha_helper env1 env2 f1 f2 && isalpha_helper env1 env2 a1 a2)
      | _, _ -> false
    in

    let my_e1 = alpha_convert(e1) in
    let my_e2 = alpha_convert(e2) in

    let env1 = create_environment my_e1 in
    let env2 = create_environment my_e2 in

    if compare_envs env1 env2 then
      isalpha_helper env1 env2 my_e1 my_e2
    else
      false
  ;;

let substitute x arg body env =
  let rec substitute_helper body =
    match body with
    | Var y when x = y -> arg
    | Var _ -> body
    | Func (y, _) when x = y -> body
    | Func (y, b) -> Func (y, substitute_helper b)
    | Application (e1, e2) ->
        let sub_e1 = substitute_helper e1 in
        let sub_e2 = substitute_helper e2 in
        Application (sub_e1, sub_e2)
  in
  substitute_helper body
;;

let rec reduce env e = 
  let rec reduce_helper expr =
    match expr with
    | Var x -> (
        match lookup env x with
        | Some a -> a
        | None -> expr
      )
    | Func (x, body) -> Func (x, reduce_helper body)
    | Application (Func (x, body), arg) ->
        let substituted = substitute x arg body env in
        reduce_helper substituted
    | Application (e1, e2) ->
        let reduced_e1 = reduce_helper e1 in
        let reduced_e2 = reduce_helper e2 in
        Application (reduced_e1, reduced_e2)
  in
  let reduced = reduce_helper e in
  if e = reduced then
    e  (* No further reduction possible *)
  else
    reduce env reduced
;;

let rec laze env e = 
let simpleFun = Func("x", Var("x")) in
let simpleFun2 = Func("x", Var("y")) in
  let rec laze_helper expr =
    match expr with
    | Var _ -> expr
    | Func (x, body) -> expr
    | Application (Func (x, body), arg) -> 
      if (isalpha (Func (x, body)) simpleFun) 
      then arg
      else (if (isalpha (Func (x, body)) simpleFun2) 
            then body
            else laze_helper (substitute x arg body env))
    | Application (e1, e2) -> (match e1, e2 with 
                              | Var _, Application (_, _) -> let laz = laze_helper e2 in Application (e1, laz)
                              | Application (_, _), Var _ -> let laz = laze_helper e1 in Application (laz, e2)
                              | Var _, Var _ -> Application (e1, e2))
  in
  laze_helper e
;;

let rec eager env e = 
  let rec eager_helper expr =
    match expr with
    | Var _ -> expr
    | Func (x, body) -> expr
    | Application (e1, e2) ->(match e1, e2 with 
                              | _, Application (Func (x, body), arg) -> let eag = eager_helper e2 in Application (e1, eag)
                              | Application (Func (x, body), arg), _ -> let eag = eager_helper e1 in Application (eag, e2)
                              | Func (x, body), Var y -> (match body with
                                                          |Var z -> if z=x then Var y else Var z
                                                          |Application (Var z1, Var z2) -> let r1 = if z1=x then Var y else Var z1 in
                                                                                   let r2 = if z2=x then Var y else Var z2 in
                                                                                   Application (r1, r2)
                              )
                              | Var _, Var _ -> Application (e1, e2))
  in
  eager_helper e
;;

let rec convert tree = 
   match tree with
  | Bool b -> if b then "(Lx.(Ly.x))" else "(Lx.(Ly.y))"
  | If (a, b, c) -> "((" ^ convert a ^ " " ^ convert b ^ ") " ^ convert c ^ ")"
  | And (a, b) -> "(((Lx.(Ly.((x y) (Lx.(Ly.y))))) " ^ convert a ^ ") " ^ convert b ^ ")"
  | Or (a, b) -> "(((Lx.(Ly.((x (Lx.(Ly.x))) y))) " ^ convert a ^ ") " ^ convert b ^ ")"
  | Not a -> "((Lx.((x (Lx.(Ly.y))) (Lx.(Ly.x)))) " ^ convert a ^ ")"
;;

let rec readable tree = 
  let rec add_parentheses str =
    "(" ^ str ^ ")"
  in
  match tree with
  | Func (x1, Func (y1, Var x2)) when x1<>y1 && x1 = x2-> "true"
  | Func (x1, Func (y1, Var y2)) when x1<>y1 && y1 = y2-> "false"
  | Application (Application (Func (x1,Func (y1,Application (Application (x2, Func (_,_)), _))), e1), e2) ->
      let readable_e1 = readable e1 in
      let readable_e2 = readable e2 in
      add_parentheses (readable_e1 ^ " or " ^ readable_e2)
  | Application (Application (Func (x1,Func (y1,Application (Application (x2, y2), _))), e1), e2) ->
      let readable_e1 = readable e1 in
      let readable_e2 = readable e2 in
      add_parentheses (readable_e1 ^ " and " ^ readable_e2)
  | Application (Application (e1, e2), e3) ->
      let readable_e1 = readable e1 in
      let readable_e2 = readable e2 in
      let readable_e3 = readable e3 in
      add_parentheses ("if " ^ readable_e1 ^ " then " ^ readable_e2 ^ " else " ^ readable_e3)
  |Application (Func (_,_), e1) -> 
      let readable_e1 = readable e1 in
      add_parentheses ("not " ^ readable_e1)
  |_ -> "other";
  
;;
