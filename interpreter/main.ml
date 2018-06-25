open Syntax
open Eval
open Typing

let rec read_eval_print env tyenv =
  print_string "# ";
  flush stdout;
  try
    let decl = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
    let ty = ty_decl tyenv decl in
    let (id, newenv, v) = eval_decl env decl in
    Printf.printf "val %s = " id;
    pp_ty ty;
    print_string " = ";
    pp_val v;
    print_newline();
    read_eval_print newenv tyenv
  with e ->
    let msg = Printexc.to_string e in
    print_string ("there was an error: " ^ msg ^ "\n");
    read_eval_print env tyenv;;

let initial_env = 
  Environment.extend "ii" (IntV 2)
    (Environment.extend "iii" (IntV 3) 
       (Environment.extend "iv" (IntV 4) 
          (Environment.extend "uso" (BoolV false) Environment.empty)))

let initial_tyenv = 
  Environment.extend "ii" TyInt
    ( Environment.extend "iii" TyInt
        ( Environment.extend "iv" TyInt
            ( Environment.extend "uso" TyBool Environment.empty)))

let _ = read_eval_print initial_env initial_tyenv
