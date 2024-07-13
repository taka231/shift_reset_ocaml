open Type
open Syntax

exception Unbound

let empty_env = []
let extend x v env = (x, v) :: env
let extend_ty x ty env = (x, ([], ty)) :: env
let lookup x env = try List.assoc x env with Not_found -> raise Unbound

let pure ty level =
  let ans = new_tyvar level in
  (ty, (ans, ans))

let rec infer_pattern pattern level =
  match pattern with
  | PInt _ -> (TyInt, [])
  | PBool _ -> (TyBool, [])
  | PVar x ->
    let tyvar = new_tyvar level in
    (tyvar, [(x, ([], tyvar))])
  | PPair (p1, p2) ->
    let t1, env1 = infer_pattern p1 level in
    let t2, env2 = infer_pattern p2 level in
    (TyPair (t1, Some t2), env1 @ env2)
  | PNil -> (TyList (new_tyvar level), [])
  | PCons (p1, p2) ->
    let t1, env1 = infer_pattern p1 level in
    let t2, env2 = infer_pattern p2 level in
    unify t2 (TyList t1);
    (t2, env1 @ env2)

let rec infer_expr tyenv expr level =
  match expr with
  | EConstInt _ -> pure TyInt level
  | EConstBool _ -> pure TyBool level
  | EUnit -> pure TyUnit level
  | EPair (e1, e2) ->
      let t1, (ans1, ans2) = infer_expr tyenv e1 level in
      let t2, (ans3, ans4) = infer_expr tyenv e2 level in
      unify ans1 ans4;
      let ans = (ans3, ans2) in
      (match t1, t2 with
      | TyVar (v1, {contents = None}, _), TyVar (v2, {contents = None}, _) when v1 = v2 -> (TyPair (t1, None), ans)
      | _, _ -> (TyPair (t1, Some t2)), ans)
  | ENil -> let a = new_tyvar level in pure (TyList a) level
  | ECons (e1, e2) ->
      let t1, (ans1, ans2) = infer_expr tyenv e1 level in
      let t2, (ans3, ans4) = infer_expr tyenv e2 level in
      unify ans1 ans4;
      let ans = (ans3, ans2) in
      unify t2 (TyList t1); (TyList t1, ans)
  | EAdd (e1, e2) | ESub (e1, e2) | EMul (e1, e2) | EDiv (e1, e2) ->
      let t1, (ans1, ans2) = infer_expr tyenv e1 level in
      let t2, (ans3, ans4) = infer_expr tyenv e2 level in
      unify ans1 ans4;
      let ans = (ans3, ans2) in
      unify t1 TyInt; unify t2 TyInt; (TyInt, ans)
  | EEq (e1, e2) | ELt (e1, e2) ->
      let t1, (ans1, ans2) = infer_expr tyenv e1 level in
      let t2, (ans3, ans4) = infer_expr tyenv e2 level in
      unify ans1 ans4;
      let ans = (ans3, ans2) in
      unify t1 t2; (TyBool, ans)
  | EAnd (e1, e2) | EOr (e1, e2) ->
      let t1, (ans1, ans2) = infer_expr tyenv e1 level in
      let t2, (ans3, ans4) = infer_expr tyenv e2 level in
      unify ans1 ans4;
      let ans = (ans3, ans2) in
      unify t1 TyBool; unify t2 TyBool; (TyBool, ans)
  | EIf (e1, e2, e3) ->
      let t1, (ans1, ans2) = infer_expr tyenv e1 level in
      let t2, (ans3, ans4) = infer_expr tyenv e2 level in
      let t3, (ans5, ans6) = infer_expr tyenv e3 level in
      unify ans3 ans5; unify ans4 ans6; unify ans1 ans4;
      let ans = (ans3, ans2) in
      unify t1 TyBool; unify t2 t3; (t2, ans)
  | EVar x ->
      let (vars, ty) = lookup x tyenv in
      pure (instantiate (vars, ty) level) level
  | EApp (e1, e2) ->
      let t1, (ans1, ans2) = infer_expr tyenv e1 level in
      let t2, (ans3, ans4) = infer_expr tyenv e2 level in
      let a = new_tyvar level in
      let fans1 = new_tyvar level in
      let fans2 = new_tyvar level in
      unify t1 (TyFun ((t2, fans1), (a, fans2)));
      unify ans1 ans4; unify fans2 ans3;
      let ans = (fans1, ans2) in
      (a, ans)
  | EFun (x, e) ->
      let a = new_tyvar level in
      let env = extend_ty x a tyenv in
      let t, (ans1, ans2) = infer_expr env e level in
      pure (TyFun ((a, ans1), (t, ans2))) level
  | ELet (name, e1, e2) ->
      let t1, (ans1, ans2) = infer_expr tyenv e1 (level+1) in
      let (vars, t1) = generalize t1 level in
      let ans1tyvars = get_tyvars ans1 in
      let ans2tyvars = get_tyvars ans2 in
      let vars = List.filter (fun x -> (not (List.mem x ans1tyvars)) && (not (List.mem x ans2tyvars))) vars in
      let env = extend name (vars, t1) tyenv in
      let t, (ans3, ans4) = infer_expr env e2 level in
      unify ans1 ans4; (t, (ans3, ans2))
  | ELetRec (f, x, e1, e2) ->
      let x_ty = new_tyvar (level+1) in
      let fans1 = new_tyvar (level+1) in
      let fans2 = new_tyvar (level+1) in
      let result_ty = new_tyvar level in
      let fun_ty = TyFun ((x_ty, fans1), (result_ty, fans2)) in
      let env = extend_ty f fun_ty tyenv in
      let t1, (ans1, ans2) = infer_expr (extend_ty x x_ty env) e1 (level+1) in
      unify t1 result_ty; unify ans1 fans1; unify ans2 fans2;
      let gen_ty = generalize fun_ty level in
      let env = extend f gen_ty tyenv in
      let t2 = infer_expr env e2 level in t2
  | EShift (k, e) ->
      let a = new_tyvar level in
      let b = new_tyvar level in
      let gen_t = new_tyvar level in
      let gen_t_num = match gen_t with
      | TyVar (v, _, _) -> v
      | _ -> raise (TyError "gen_t is not TyVar") in
      let k_ty = ([gen_t_num], TyFun ((a, gen_t), (b, gen_t))) in
      let env = extend k k_ty tyenv in
      let t, (ans1, ans2) = infer_expr env e level in
      unify ans1 t;
      (a, (b, ans2))
  | EReset e ->
      let t, (ans1, ans2) = infer_expr tyenv e level in
      unify ans1 t;
      pure ans2 level
  | EMatch (e, patterns) ->
      let t, (ans1, ans2) = infer_expr tyenv e level in
      let pattern_list = List.map (fun (pattern, _) -> infer_pattern pattern level) patterns in
      let alpha = new_tyvar level in
      let ans3 = new_tyvar level in
      let ans4 = new_tyvar level in
      List.iter2 (fun (pattern_ty, env') (_, e) ->
        unify t pattern_ty;
        let t', (ans3', ans4') = infer_expr (env' @ tyenv) e level in
        unify t' alpha;
        unify ans3 ans3'; unify ans4 ans4';
      ) pattern_list patterns;
      unify ans1 ans4;
      let ans = (ans3, ans2) in
      (alpha, ans)

let infer_cmd tyenv cmd =
  match cmd with
  | CExp e ->
      let t, (ans1, ans2) = infer_expr tyenv e 0 in
      (try
        unify ans1 ans2; (* check e is pure *)
        (([], t), tyenv)
      with _ -> raise (TyError "not pure"))
  | CDecl (x, e) ->
      let t, (ans1, ans2) = infer_expr tyenv e 1 in
      (try
        unify ans1 ans2; (* check e is pure *)
        let ty = generalize t 0 in
        (ty, (x, ty)::tyenv)
      with _ -> raise (TyError "not pure"))
  | CRecDecl (f, x, e) ->
      let x_ty = new_tyvar 1 in
      let fans1 = new_tyvar 1 in
      let fans2 = new_tyvar 1 in
      let result_ty = new_tyvar 0 in
      let fun_ty = TyFun ((x_ty, fans1), (result_ty, fans2)) in
      let env = extend_ty f fun_ty tyenv in
      let t, (ans1, ans2) = infer_expr (extend_ty x x_ty env) e 1 in
      unify t result_ty; unify ans1 fans1; unify ans2 fans2;
      let gen_ty = generalize fun_ty 0 in
      (gen_ty, (f, gen_ty)::tyenv)

let rec infer_cmds tyenv cmds =
  match cmds with
  | [] -> ([], tyenv)
  | c :: cs ->
      let msg, env = infer_cmd tyenv c in
      let msgs, env = infer_cmds env cs in
      (msg :: msgs, env)
