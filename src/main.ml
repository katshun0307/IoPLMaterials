open Syntax
open Eval

let rec read_eval_print env =
  print_string "# ";
  flush stdout;
  let decl = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
  let (id, newenv, v) = eval_decl env decl in
  Printf.printf "val %s = " id;
  pp_val v;
  print_newline();
  read_eval_print newenv

let initial_env = 
  Environment.extend "hoge" (IntV 1)
    (Environment.extend "fuga" (IntV 5) 
       (Environment.extend "piyo" (IntV 10) 
          (Environment.extend "hona" (BoolV false) Environment.empty)))

let _ = read_eval_print initial_env
