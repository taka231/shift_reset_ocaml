open Syntax
open Eval
open Infer
open Type

let rec read_eval_print (env, tyenv) =
  print_string "# ";
  flush stdout;
  read_eval_print @@ try
    let cmd = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
    let tys, tyenv = infer_cmds tyenv cmd in
    let msgs, env = eval_commands_k env cmd in
    let _ = List.map2 (fun (msg, v) ty -> Printf.printf "%s : " msg; print_ty_schema ty; Printf.printf " = " ; print_value v; print_newline ()) msgs tys in
    (env, tyenv)
  with
    | EvalErr msg -> Printf.printf "%s\n" msg; (env, tyenv)
    | Failure err when err = "lexing: empty token" -> Printf.printf "\nbye!"; exit 0
    | Failure err -> Printf.printf "Error: %s\n" err; (env, tyenv)
    | TyError msg -> Printf.printf "Type error: %s\n" msg; (env, tyenv)
    | Unbound -> Printf.printf "variable unbound\n"; (env, tyenv)
    | ConstraintSolver.TyError -> Printf.printf "Type error\n"; (env, tyenv)
    | Parser.Error -> Printf.printf "Parse error\n"; (env, tyenv)

let initial_env =
  empty_env

let _ = read_eval_print (initial_env, empty_env)
