open Syntax 

(* 値の定義 *)

(* exval は式を評価して得られる値．dnval は変数と紐付けられる値．今回
   の言語ではこの両者は同じになるが，この2つが異なる言語もある．教科書
   参照． *)
type exval =
  | IntV of int
  | BoolV of bool
  | ProcV of id * exp * dnval Environment.t
  | DProcV of id * exp
  | RProcV of id * exp * dnval Environment.t ref
and dnval = exval

exception Error of string

let err s = raise (Error s)

(* pretty printing *)
let rec string_of_exval = function
    IntV i -> string_of_int i
  | BoolV b -> string_of_bool b
  | ProcV _ -> "some static function"
  | DProcV _ -> "some dynamic function"
  | RProcV _ -> "some recursive function"

let pp_val v = print_string (string_of_exval v)

let rec apply_prim op arg1 arg2 = match op, arg1, arg2 with
    Plus, IntV i1, IntV i2 -> IntV (i1 + i2)
  | Plus, _, _ -> err ("Both arguments must be integer: +")
  | Mult, IntV i1, IntV i2 -> IntV (i1 * i2)
  | Mult, _, _ -> err ("Both arguments must be integer: *")
  | Lt, IntV i1, IntV i2 -> BoolV (i1 < i2)
  | Lt, _, _ -> err ("Both arguments must be integer: <")
  | And, BoolV i1, BoolV i2 -> BoolV(i1 && i2)
  | And, _, _ -> err ("Both arguments must be boolean: &&")
  | Or, BoolV i1, BoolV i2 -> BoolV(i1 || i2)
  | Or, _, _ -> err("Both arguments must be boolean: ||")

let rec eval_exp env = function
    Var x -> 
    (try Environment.lookup x env with 
       Environment.Not_bound -> err ("Variable not bound: " ^ x))
  | ILit i -> IntV i
  | BLit b -> BoolV b
  | BinOp (op, exp1, exp2) -> 
    let arg1 = eval_exp env exp1 in
    let arg2 = eval_exp env exp2 in
    apply_prim op arg1 arg2
  | IfExp (exp1, exp2, exp3) ->
    let test = eval_exp env exp1 in
    (match test with
     | BoolV true -> eval_exp env exp2 
     | BoolV false -> eval_exp env exp3
     | _ -> err ("Test expression must be boolean: if"))
  | LetExp (id, exp1, exp2) ->
    let value = eval_exp env exp1 in 
    eval_exp (Environment.extend id value env) exp2
  | FunExp (id, exp) -> ProcV (id, exp, env) (* save current environment "env" inside closure *)
  | AppExp (exp1, exp2) -> (
      let funval = eval_exp env exp1 in
      let arg = eval_exp env exp2 in 
      (match funval with
       | ProcV (id, body, env') -> 
         let newenv = Environment.extend id arg env' in
         eval_exp newenv body
       | DProcV(id, body) -> eval_exp env body
       | e -> err ("Non function value is applied")))
  | DFunExp (id, exp) -> DProcV(id, exp)
  | _ -> err("not able to calculate")

(* | LetRecExp (id, para, exp1, exp2) ->
   (* make reference to dummy environment *)
   let dummyenv = ref Environment.empty in
   let newenv = Environment.extend id (RProcV(para, exp1, dummyenv)) env in
   dummyenv := newenv;
   eval_exp newenv exp2 *)

let rec eval_decl env = function
    Exp e -> let v = eval_exp env e in ("-", env, v)
  | Decl (id, e) -> let v = eval_exp env e in (id, Environment.extend id v env, v)
  | DeclList (l1, l2) -> (match l2 with
      | DeclListEnd _ -> eval_decl env l1
      | _ -> let (id, new_env, v) = eval_decl env l1 in eval_decl new_env l2)
  | ClosedDeclList(lst) ->
    let rec loop current_env const_env l =
      (match l with 
       | top :: [] -> 
         (match top with
          | ClosedDecl(id, e) -> let v = eval_exp const_env e  in 
            (id, Environment.extend id v current_env, v)
          | ClosedLetExp(id, e1, e2) -> let value = eval_exp const_env e1 in 
            let res = eval_exp (Environment.extend id value env) e2 in 
            (id, Environment.extend id res current_env, res)
          | _ -> err("failed"))
       | top :: rest -> 
         (match top with
          | ClosedDecl(id, e) -> let v = eval_exp const_env e  in 
            let (_, res_env, _) = loop current_env const_env rest in
            (id, Environment.extend id v res_env, v)
          | ClosedLetExp(id, e1, e2) -> let value = eval_exp const_env e1 in 
            let v = eval_exp (Environment.extend id value env) e2 in
            let (_, res_env, _) = loop current_env const_env rest in
            (id, Environment.extend id v res_env, v)
          | _ -> err("failed"))
       | [] -> err("failed")
      ) in loop env env lst
  | _ -> err("failed")
