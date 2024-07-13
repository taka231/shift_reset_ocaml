open Syntax
open Infer

exception EvalErr of string

let rec eval_expr_k env e cont =
  match e with
  | EConstInt i -> cont (VInt i)
  | EConstBool b -> cont (VBool b)
  | EPair (e1, e2) ->
      eval_expr_k env e1 (fun v1 ->
      eval_expr_k env e2 (fun v2 -> (cont (VPair (v1, v2)))))
  | ENil -> cont (VList [])
  | ECons (e1, e2) ->
      eval_expr_k env e1 (fun v1 ->
      eval_expr_k env e2 (fun v2 ->
      match v2 with
      | VList vs -> cont (VList (v1 :: vs))
      | _ -> raise @@ EvalErr "Type error"))
  | EVar x -> (
      try cont (lookup x env) with Unbound -> raise @@ EvalErr "Unbound variable")
  | EAdd (e1, e2) ->
      eval_expr_k env e1 (fun v1 ->
      eval_expr_k env e2 (fun v2 ->
      match (v1, v2) with
      | VInt i1, VInt i2 -> cont (VInt (i1 + i2))
      | _ -> raise @@ EvalErr "Type error"))
  | ESub (e1, e2) ->
      eval_expr_k env e1 (fun v1 ->
      eval_expr_k env e2 (fun v2 ->
      match (v1, v2) with
      | VInt i1, VInt i2 -> cont (VInt (i1 - i2))
      | _ -> raise @@ EvalErr "Type error"))
  | EMul (e1, e2) ->
      eval_expr_k env e1 (fun v1 ->
      eval_expr_k env e2 (fun v2 ->
      match (v1, v2) with
      | VInt i1, VInt i2 -> cont (VInt (i1 * i2))
      | _ -> raise @@ EvalErr "Type error"))
  | EDiv (e1, e2) ->
      eval_expr_k env e1 (fun v1 ->
      eval_expr_k env e2 (fun v2 ->
      match (v1, v2) with
      | VInt i1, VInt i2 -> cont (VInt (i1 / i2))
      | _ -> raise @@ EvalErr "Type error"))
  | EEq (e1, e2) ->
      eval_expr_k env e1 (fun v1 ->
      eval_expr_k env e2 (fun v2 ->
      match (v1, v2) with
      | VInt i1, VInt i2 -> cont (VBool (i1 = i2))
      | VBool b1, VBool b2 -> cont (VBool (b1 = b2))
      | _ -> raise @@ EvalErr "Type error"))
  | ELt (e1, e2) ->
      eval_expr_k env e1 (fun v1 ->
      eval_expr_k env e2 (fun v2 ->
      match (v1, v2) with
      | VInt i1, VInt i2 -> cont (VBool (i1 < i2))
      | VBool b1, VBool b2 -> cont (VBool (b1 < b2))
      | _ -> raise @@ EvalErr "Type error"))
  | EAnd (e1, e2) ->
      eval_expr_k env e1 (fun v1 ->
      eval_expr_k env e2 (fun v2 ->
      match (v1, v2) with
      | VBool b1, VBool b2 -> cont (VBool (b1 && b2))
      | _ -> raise @@ EvalErr "Type error"))
  | EOr (e1, e2) ->
      eval_expr_k env e1 (fun v1 ->
      eval_expr_k env e2 (fun v2 ->
      match (v1, v2) with
      | VBool b1, VBool b2 -> cont (VBool (b1 || b2))
      | _ -> raise @@ EvalErr "Type error"))
  | EIf (e1, e2, e3) ->
      eval_expr_k env e1 (fun v1 ->
      match v1 with
      | VBool b -> eval_expr_k env (if b then e2 else e3) cont
      | _ -> raise @@ EvalErr "Type error")
  | ELet (name, e1, e2) ->
      eval_expr_k env e1 (fun v ->
      let env' = extend name v env in
      eval_expr_k env' e2 cont)
  | ELetRec (f, x, e1, e2) ->
      let envRef = ref env in
      let v = VFun (x, e1, envRef) in
      let env' = extend f v env in
      envRef := env';
      eval_expr_k env' e2 cont
  | EFun (x, e) -> cont (VFun (x, e, ref env))
  | EApp (e1, e2) ->
      eval_expr_k env e1 (fun v1 ->
      eval_expr_k env e2 (fun v2 ->
      match v1 with
      | VFun (x, e, env') ->
          let env'' = extend x v2 !env' in
          eval_expr_k env'' e cont
      | VCont cont' -> cont (cont' v2)
      | _ -> raise @@ EvalErr "Type error"))
  | EUnit -> cont VUnit
  | EReset e ->
      let v = eval_expr_k env e (fun v -> v) in
      cont v
  | EShift (k, e) ->
      let env' = extend k (VCont cont) env in
      eval_expr_k env' e (fun x -> x)
  | EMatch (e, patterns) ->
      eval_expr_k env e (fun v ->
      let rec is_match v pattern =
        match pattern with
        | PInt i -> (
          match v with
          | VInt j -> (i = j, [])
          | _ -> (false, []))
        | PBool b -> (
          match v with
          | VBool c -> (b = c, [])
          | _ -> (false, []))
        | PVar x -> (true, [(x, v)])
        | PPair (p1, p2) -> (
          match v with
          | VPair (v1, v2) ->
              let (is_match1, env1) = is_match v1 p1 in
              let (is_match2, env2) = is_match v2 p2 in
              (is_match1 && is_match2, env1 @ env2)
          | _ -> (false, []))
        | PNil -> (
          match v with
          | VList [] -> (true, [])
          | _ -> (false, []))
        | PCons (p1, p2) -> (
          match v with
          | VList (v1 :: v2) ->
              let (is_match1, env1) = is_match v1 p1 in
              let (is_match2, env2) = is_match (VList v2) p2 in
              (is_match1 && is_match2, env1 @ env2)
          | _ -> (false, []))
      in
      let rec match_pattern pattern_e_list =
        match pattern_e_list with
        | [] -> raise @@ EvalErr "Pattern match failure"
        | (pattern, e) :: pattern_e_list ->
            let (is_match, env') = is_match v pattern in
            if is_match then eval_expr_k (env' @ env) e cont
            else match_pattern pattern_e_list
      in
      match_pattern patterns)

let eval_command_k env c =
  match c with
  | CExp e -> 
      let v = eval_expr_k env e (fun v -> v) in
      (("-", v), env)
  | CDecl (x, e) ->
      let v = eval_expr_k env e (fun v -> v) in
      (("val " ^ x, v), extend x v env)
  | CRecDecl (f, x, e) ->
      let envRef = ref env in
      let v = VFun (x, e, envRef) in
      let env' = extend f v env in
      envRef := env';
      (("val " ^ f, v), env')

let rec eval_commands_k env cs =
  match cs with
  | [] -> ([], env)
  | c :: cs ->
      let msg, env = eval_command_k env c in
      let msgs, env = eval_commands_k env cs in
      (msg :: msgs, env)

