type tyvar = int

type ty =
  | TyInt
  | TyBool
  | TyUnit
  | TyFun of (ty * ty) * (ty * ty)
  | TyVar of tyvar * ty option ref * int ref
  | TyPair of ty * ty option
  | TyList of ty

type type_schema = tyvar list * ty

let new_tyvar =
  let counter = ref 0 in
  fun level ->
    let v = !counter in
    let ty_ref = ref None in
    let level_ref = ref level in
    counter := v + 1;
    TyVar (v, ty_ref, level_ref)

let rec print_ty ty =
  match ty with
  | TyInt -> print_string "int"
  | TyBool -> print_string "bool"
  | TyUnit -> print_string "unit"
  | TyFun ((ty1, ans1), (ty2, ans2)) ->
      print_string "(";
      print_ty ty1;
      print_string "/";
      print_ty ans1;
      print_string " -> ";
      print_ty ty2;
      print_string "/";
      print_ty ans2;
      print_string ")"
  | TyVar (v, { contents = None }, _) -> print_string ("'a" ^ string_of_int v)
  | TyVar (_, { contents = Some ty }, _) -> print_ty ty
  | TyPair (ty1, Some ty2) ->
      print_string "(";
      print_ty ty1;
      print_string " * ";
      print_ty ty2;
      print_string ")"
  | TyPair (ty1, None) ->
      print_string "(";
      print_ty ty1;
      print_string " * ";
      print_ty ty1;
      print_string ")"
  | TyList ty ->
      print_string "(";
      print_ty ty;
      print_string " list)"

let print_ty_schema (vars, ty) =
  match vars with
  | [] -> print_ty ty
  | vars ->
      print_string "forall";
      List.iter (fun v -> print_string (" 'a" ^ string_of_int v)) vars;
      print_string ". ";
      print_ty ty

let rec occurs v ty =
  match ty with
  | TyInt | TyBool | TyUnit -> false
  | TyFun ((ty1, ans1), (ty2, ans2)) ->
      occurs v ty1 || occurs v ty2 || occurs v ans1 || occurs v ans2
  | TyVar (v', { contents = None }, _) -> v = v'
  | TyVar (_, { contents = Some ty }, _) -> occurs v ty
  | TyPair (ty1, Some ty2) -> occurs v ty1 || occurs v ty2
  | TyPair (ty1, None) -> occurs v ty1
  | TyList ty -> occurs v ty

let rec ty_subst_var subst ty =
  match ty with
  | TyInt | TyBool | TyUnit -> ty
  | TyFun ((ty1, ans1), (ty2, ans2)) ->
      TyFun
        ( (ty_subst_var subst ty1, ty_subst_var subst ans1),
          (ty_subst_var subst ty2, ty_subst_var subst ans2) )
  | TyPair (ty1, Some ty2) ->
      TyPair (ty_subst_var subst ty1, Some (ty_subst_var subst ty2))
  | TyPair (ty1, None) -> TyPair (ty_subst_var subst ty1, None)
  | TyVar (v, { contents = None }, _) ->
      List.assoc_opt v subst |> Option.value ~default:ty
  | TyVar (v, { contents = Some ty }, level) ->
      TyVar (v, ref (Some (ty_subst_var subst ty)), level)
  | TyList ty -> TyList (ty_subst_var subst ty)

let instantiate (vars, ty) level =
  let subst = List.map (fun v -> (v, new_tyvar level)) vars in
  if subst = [] then ty else ty_subst_var subst ty

let rec get_tyvars ty =
  match ty with
  | TyInt | TyBool | TyUnit -> []
  | TyFun ((ty1, ans1), (ty2, ans2)) ->
      let tyvars1 = get_tyvars ty1 in
      let tyvars2 = get_tyvars ty2 in
      let tyvars = tyvars1 @ List.filter (fun v -> not (List.mem v tyvars1)) tyvars2 in
      let tyvars_ans1 = get_tyvars ans1 in
      let tyvars = tyvars @ List.filter (fun v -> not (List.mem v tyvars)) tyvars_ans1 in
      let tyvars_ans2 = get_tyvars ans2 in
      let tyvars = tyvars @ List.filter (fun v -> not (List.mem v tyvars)) tyvars_ans2 in
      tyvars
  | TyPair (ty1, None) -> get_tyvars ty1
  | TyPair (ty1, Some ty2) ->
      let tyvars1 = get_tyvars ty1 in
      tyvars1 @ (get_tyvars ty2 |> List.filter (fun v -> not (List.mem v tyvars1)))
  | TyVar (_, { contents = Some ty }, _) -> get_tyvars ty
  | TyVar (v, _, _) -> [ v ]
  | TyList ty -> get_tyvars ty

let generalize ty level =
  let rec collect_tyvars ty =
    match ty with
    | TyInt | TyBool | TyUnit -> []
    | TyPair (ty1, None) -> collect_tyvars ty1
    | TyList ty -> collect_tyvars ty
    | TyFun ((ty1, ans1), (ty2, ans2)) ->
        let tyvars1 = collect_tyvars ty1 in
        let tyvars2 = collect_tyvars ty2 in
        let tyvars = tyvars1 @ List.filter (fun v -> not (List.mem v tyvars1)) tyvars2 in
        let tyvars_ans1 = collect_tyvars ans1 in
        let tyvars = tyvars @ List.filter (fun v -> not (List.mem v tyvars)) tyvars_ans1 in
        let tyvars_ans2 = collect_tyvars ans2 in
        let tyvars = tyvars @ List.filter (fun v -> not (List.mem v tyvars)) tyvars_ans2 in
        tyvars
    | TyPair (ty1, Some ty2) ->
        let tyvars1 = collect_tyvars ty1 in
        tyvars1
        @ (collect_tyvars ty2 |> List.filter (fun v -> not (List.mem v tyvars1)))
    | TyVar (_, { contents = Some ty }, _) -> collect_tyvars ty
    | TyVar (v, _, l) -> if !l > level then [ v ] else []
  in
  let tyvars = collect_tyvars ty in
  (tyvars, ty)

exception TyError of string

let rec unify ty1 ty2 =
  match (ty1, ty2) with
  | TyPair (ty11, None), TyPair (ty21, None) -> unify ty11 ty21
  | TyFun ((ty11, ans11), (ty12, ans12)), TyFun ((ty21, ans21), (ty22, ans22)) ->
      unify ty11 ty21;
      unify ans11 ans21;
      unify ty12 ty22;
      unify ans12 ans22
  | TyPair (ty11, Some ty12), TyPair (ty21, Some ty22) ->
      unify ty11 ty21;
      unify ty12 ty22
  | TyList ty1, TyList ty2 -> unify ty1 ty2
  | TyVar (v1, { contents = None }, _), TyVar (v2, { contents = None }, _)
    when v1 = v2 ->
      ()
  | TyVar (_, r1, l1), TyVar (_, { contents = None }, l2)
    when Option.is_none !r1 ->
      if !l1 > !l2 then l2 := !l1 else l1 := !l2;
      r1 := Some ty2
  | TyVar (_, { contents = Some ty1 }, _), _ -> unify ty1 ty2
  | _, TyVar (_, { contents = Some ty2 }, _) -> unify ty1 ty2
  | TyVar (v, r, _), ty | ty, TyVar (v, r, _) ->
      if occurs v ty then raise (TyError "occur check failed") else r := Some ty
  | TyInt, TyInt | TyBool, TyBool -> ()
  | _, _ -> raise (TyError "unify failed")
