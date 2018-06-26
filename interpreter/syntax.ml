(* ML interpreter / type reconstruction *)
type id = string

type binOp = Plus | Mult | Lt | And | Or
type logicOp = And | Or

type exp =
  | Var of id (* Var "x" --> x *)
  | ILit of int (* ILit 3 --> 3 *)
  | BLit of bool (* BLit true --> true *)
  | BinOp of binOp * exp * exp
  | LogicOp of logicOp * exp * exp
  (* BinOp(Plus, ILit 4, Var "x") --> 4 + x *)
  | IfExp of exp * exp * exp
  (* IfExp(BinOp(Lt, Var "x", ILit 4), 
           ILit 3, 
           Var "x") --> 
     if x<4 then 3 else x *)
  | MultiLetExp of (id * exp) list * exp  
  | FunExp of id list * exp (* static function expression *)
  | DFunExp of id * exp (* dynamic function expression *)
  | AppExp of exp * exp (* function application expression *)
(* | LetRecExp of id * id * exp * exp*) (* recursive function expression *)
(* let rec id =
   fun id -> exp in exp *)

type program =
    Exp of exp
  | Decl of id * exp
  (* | RecDecl of id * id * exp *)

type ty = TyInt | TyBool

let pp_ty = function
  | TyInt -> print_string "int"
  | TyBool -> print_string "bool"
