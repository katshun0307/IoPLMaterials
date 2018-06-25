(* ML interpreter / type reconstruction *)
type id = string

type binOp = Plus | Mult | Lt

let string_of_binop = function
  | Plus -> "Plus"
  | Mult -> "Mult"
  | Lt -> "Lt"

type logicOp = And | Or

type exp =
  | Var of id (* Var "x" --> x *)
  | ILit of int (* ILit 3 --> 3 *)
  | BLit of bool (* BLit true --> true *)
  | BinOp of binOp * exp * exp
  (* BinOp(Plus, ILit 4, Var "x") --> 4 + x *)
  | LogicOp of logicOp * exp * exp 
  | IfExp of exp * exp * exp
  (* IfExp(BinOp(Lt, Var "x", ILit 4), 
           ILit 3, 
           Var "x") --> 
     if x<4 then 3 else x *)
  (* | LetExp of id * exp * exp let expression *)
  | MultiLetExp of (id * exp) list * exp
  | FunExp of id list * exp (* static function expression *)
  | DFunExp of id * exp (* dynamic function expression *)
  | AppExp of exp * exp (* function application expression *)
  | LetRecExp of id * id * exp * exp (* recursive function expression *)
(* let rec id =
   fun id -> exp in exp *)

type program =
    Exp of exp
  | Decl of id * exp
  | DeclList of (id * exp) list
  | ClosedDecl of id * exp
  | ClosedLetExp of id * exp * exp
  | ClosedDeclList of program list
  | RecDecl of id * id * exp
