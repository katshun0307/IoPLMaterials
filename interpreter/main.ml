open Syntax
open Eval

let debug = false

(* for debug *)
let rec string_of_exp e = 
  match e with
  | Var i -> "var(" ^ i ^ ")"
  | ILit i -> "int(" ^ string_of_int i ^ ")"
  | BLit i -> "boolean(" ^ string_of_bool i ^ ")"
  | BinOp(op, e1, e2) -> 
    "binop(" ^ string_of_binop op ^ ", " ^ string_of_exp e1 ^ ", " ^ string_of_exp e2 ^ ")"
  | LogicOp _ -> "logicOp"
  | IfExp _ -> "ifexp"
  (* | LetExp(i, e1, e2) -> "let(" ^ i ^ ", " ^ string_of_exp e1 ^ ", " ^ string_of_exp e2 ^ ")" *)
  | MultiLetExp _ -> "multiletexp"
  (* | FunExp(i, e) -> "fun(" ^ i ^ ", " ^ string_of_exp e  ^ ")" *)
  | DFunExp(i, e) -> "dfun(" ^ i ^ ", " ^ string_of_exp e  ^ ")"
  | AppExp(e1, e2) -> "appexp(" ^ string_of_exp e1 ^ ", " ^ string_of_exp e2  ^ ")"
  | LetRecExp (_) -> "let rec exp"
  | _ -> "others"

let rec string_of_program p = 
  match p with
  | Exp e -> print_string ("expression:" ^ string_of_exp e)
  | Decl(id, e) -> print_string ("Decl(" ^ id ^ ":" ^ string_of_exp e ^ ")")
  (* | DeclList(l) -> string_of_program p1; string_of_program p2; *)
  | DeclList(_) -> print_string("decllist")
  | ClosedDecl(id, _) -> print_string ("ClosedDecl(" ^ id ^ ")")
  | ClosedLetExp(id, _, _) -> print_string ("ClosedLetExp(" ^ id ^ ")")
  | ClosedDeclList(lst) -> let rec loop = function
      | top :: rest -> string_of_program top;
        loop rest
      | [] -> print_string" " in loop lst
  (* | RecDecl _ -> print_string "recdecl" *)
  | RecDecl(id1, id2, _) -> print_string ("RecDecl(" ^ id1 ^ ", (" ^ id2 ^ "))")

let rec print_decls decls = 
  match decls with 
  | top :: rest -> let (id, new_env, v) = top in 
    Printf.printf "val %s = " id;
    pp_val v;
    print_newline();
    print_decls rest
  | [] -> print_string ""

let rev list =
  let rec aux acc = function
    | [] -> acc
    | h::t -> aux (h::acc) t in
  aux [] list

let rec find l n = 
  match l with 
  | top :: rest -> if top = n then true else find rest n
  | [] -> false

let purify_decls lst =
  let rec loop l defined res = 
    match l with 
    | top :: rest -> let (id, _, _) = top in
      if find defined id then loop rest defined res
      else loop rest (id :: defined) (top :: res)
    | [] -> res
  in loop lst [] [] 

let get_newenv decls = 
  match decls with
  | top :: rest -> let (_, new_env, _) = top in new_env
  | [] -> err("failed to get newenv")

let rec read_eval_print env =
  print_string "# ";
  flush stdout;
  try
    let decl = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
    let declarations = eval_decls env decl in
    let newenv = get_newenv declarations in
    print_decls  (purify_decls declarations);
    (* debug *)
    (* string_of_program decl; *)
    (* print_newline(); *)
    (* debug *)
    (* Printf.printf "val %s = " id; *)
    (* pp_val v;*)
    (* print_newline(); *)
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

let read_file filename = 
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true; do
      lines := input_line chan :: !lines
    done; !lines
  with End_of_file ->
    close_in chan;
    List.rev !lines ;;

let rec string_of_list = function
  | top :: rest -> top ^ ", " ^ string_of_list rest
  | [] -> ""

let rec batch_interpreter env l = 
  match l with 
  | top :: rest -> print_string top; print_newline();
    let decl = Parser.toplevel Lexer.main (Lexing.from_string top) in
    let (id, newenv, v) = eval_decl env decl in
    Printf.printf "val %s = " id;
    pp_val v; 
    print_newline();
    batch_interpreter newenv rest
  | [] -> read_eval_print env

let _ = 
  if Array.length Sys.argv = 1 
  then read_eval_print initial_env;
  if Array.length Sys.argv = 2 
  then 
    (print_string ("reading : " ^ Sys.argv.(1));
     print_newline();
     batch_interpreter initial_env ( read_file Sys.argv.(1) );)
(* normal exit: all channels are flushed and closed *)
(* read_eval_print initial_env; *)