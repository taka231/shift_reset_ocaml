type name = string

type pattern =
  | PInt  of int
  | PBool of bool
  | PVar  of name
  | PPair of pattern * pattern
  | PNil
  | PCons of pattern * pattern

type value =
  | VInt  of int
  | VBool of bool
  | VFun  of name * expr * env ref
  | VPair of value * value
  | VUnit
  | VList of value list
  | VCont of (value -> value)

and expr =
  | EConstInt  of int
  | EConstBool of bool
  | EVar       of name
  | EAdd       of expr * expr
  | ESub       of expr * expr
  | EMul       of expr * expr
  | EDiv       of expr * expr
  | EEq        of expr * expr
  | ELt        of expr * expr
  | EAnd       of expr * expr
  | EOr        of expr * expr
  | EFun       of name * expr
  | EApp       of expr * expr
  | EIf        of expr * expr * expr
  | ELet       of name * expr * expr
  | ELetRec    of name * name * expr * expr
  | EPair      of expr * expr
  | EUnit
  | ENil
  | ECons      of expr * expr
  (* reset e *)
  | EReset     of expr
  (* shift k e *)
  | EShift     of (name * expr)
  | EMatch     of expr * (pattern * expr) list

and env = (name * value) list

type command =
  | CExp  of expr
  | CDecl of name * expr
  | CRecDecl of name * name * expr

let print_name = print_string

let rec print_pattern p =
  match p with
  | PInt i -> print_int i
  | PBool b -> print_string (string_of_bool b)
  | PVar x -> print_string x
  | PPair (p1, p2) ->
     (print_string "(";
      print_pattern p1;
      print_string ",";
      print_pattern p2;
      print_string ")")
  | PNil -> print_string "[]"
  | PCons (p1, p2) ->
     (print_pattern p1;
      print_string "::";
      print_pattern p2)

let rec print_value v =
  match v with
  | VInt i  -> print_int i
  | VBool b -> print_string (string_of_bool b)
  | VFun (_, _, _) ->
      print_string "<fun>"
  | VPair (v1, v2) ->
      print_string "(";
      print_value v1;
      print_string ",";
      print_value v2;
      print_string ")"
  | VCont _ ->
      print_string "<cont>"
  | VUnit ->
      print_string "()"
  | VList vs ->
      print_string "[";
      List.iter
        (fun v ->
           print_value v;
           print_string ";")
        vs;
      print_string "]"

(*
 小さい式に対しては以下でも問題はないが，
 大きいサイズの式を見やすく表示したければ，Formatモジュール
   https://ocaml.org/api/Format.html
 を活用すること
*)
let rec print_expr e =
  match e with
  | EConstInt i ->
      print_int i
  | EConstBool b ->
      print_string (string_of_bool b)
  | EVar x ->
      print_name x
  | EAdd (e1,e2) ->
      print_string "EAdd (";
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")"
  | ESub (e1,e2) ->
      print_string "ESub (";
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")"
  | EMul (e1,e2) ->
      print_string "EMul (";
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")"
  | EDiv (e1,e2) ->
      print_string "EDiv (";
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")"
  | EEq (e1,e2) ->
      print_string "EEq (";
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")"
  | ELt (e1, e2) ->
      print_string "ELt (";
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")"
  | EIf (e1,e2,e3) ->
      print_string "EIf (";
      print_expr   e1;
      print_string ",";
      print_expr   e2;
      print_string ",";
      print_expr   e3;
      print_string ")"
  | ELet (name, e1, e2) ->
      print_string "ELet (";
      print_name name;
      print_string ",";
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")"
  | ELetRec (f, x, e1, e2) ->
      print_string "ELetRec (";
      print_name f;
      print_string ",";
      print_name x;
      print_string ",";
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")"
  | EFun (x, e) ->
      print_string "EFun (";
      print_name x;
      print_string ",";
      print_expr e;
      print_string ")"
  | EApp (e1, e2) ->
      print_string "EApp (";
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")"
  | EAnd (e1, e2) ->
      print_string "EAnd (";
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")"
  | EOr (e1, e2) ->
      print_string "EOr (";
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")"
  | EPair (e1, e2) ->
      print_string "EPair (";
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")"
  | ENil ->
      print_string "ENil"
  | ECons (e1, e2) ->
      print_string "ECons (";
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")"
  | EUnit ->
      print_string "()"
  | EReset e ->
      print_string "EReset (";
      print_expr e;
      print_string ")"
  | EShift (x, e) ->
      print_string "EShift (";
      print_name x;
      print_string ",";
      print_expr e;
      print_string ")"
  | EMatch (e, patterns) ->
      print_string "EMatch (";
      print_expr e;
      print_string ",[";
      List.iter
        (fun (p, e) ->
           print_pattern p;
           print_string " -> ";
           print_expr e;
           print_string ";")
        patterns;
      print_string "]"

let print_command p =
  match p with
  | CExp e -> print_expr e
  | CDecl (x, e) ->
      print_string "CDecl (";
      print_name x;
      print_string ",";
      print_expr e;
      print_string ")"
  | CRecDecl (f, x, e) ->
      print_string "CRecDecl (";
      print_name f;
      print_string ",";
      print_name x;
      print_string ",";
      print_expr e;
      print_string ")"
