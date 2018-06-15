open Syntax
open Eval

let rec string_of_program p = 
  match p with
  | Exp _ -> print_string "some expression"
  | Decl(id, _) -> print_string ("Decl(" ^ id ^ ")")
  | DeclList(p1, p2) -> string_of_program p1; string_of_program p2;
  | ClosedDecl(id, _) -> print_string ("ClosedDecl(" ^ id ^ ")")
  | ClosedLetExp(id, _, _) -> print_string ("ClosedLetExp(" ^ id ^ ")")
  | ClosedDeclList(lst) -> let rec loop = function
      | top :: rest -> string_of_program top;
        loop rest 
      | [] -> print_string" " in loop lst
  | DeclListEnd _ -> print_string "DecllistEnd"
  | RecDecl(id1, id2, _) -> print_string ("RecDecl(" ^ id1 ^ ", " ^ id2 ^ ")")

let rec read_eval_print env =
  print_string "# ";
  flush stdout;
  try
    let decl = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
    let (id, newenv, v) = eval_decl env decl in
    Printf.printf "val %s = " id;
    pp_val v; 
    print_newline();
    read_eval_print newenv
  with
    e -> 
    let msg = Printexc.to_string e in
    print_string ("there was an error: " ^ msg ^ "\n");
    read_eval_print env;;


let initial_env = 
  Environment.extend "ii" (IntV 2)
    (Environment.extend "iii" (IntV 3) 
       (Environment.extend "iv" (IntV 4) 
          (Environment.extend "uso" (BoolV false) Environment.empty)))

let _ = if Array.length Sys.argv = 1 
  then read_eval_print initial_env;
else print_string Sys.argv.(1);
print_newline();