(* ML interpreter / type reconstruction *)
type id = string

type binOp = Plus | Mult | Lt | And | Or

type meta = End

type exp =
  | Var of id (* Var "x" --> x *)
  | ILit of int (* ILit 3 --> 3 *)
  | BLit of bool (* BLit true --> true *)
  | BinOp of binOp * exp * exp
  (* BinOp(Plus, ILit 4, Var "x") --> 4 + x *)
  | IfExp of exp * exp * exp
  (* IfExp(BinOp(Lt, Var "x", ILit 4), 
           ILit 3, 
           Var "x") --> 
     if x<4 then 3 else x *)
  | LetExp of id * exp * exp (* let expression *)
  | FunExp of id * exp (* static function expression *)
  | DFunExp of id * exp (* dynamic function expression *)
  | AppExp of exp * exp (* function application expression *)
  | LetRecExp of id * id * exp * exp (* recursive function expression *)
(* let rec id =
   fun id -> exp in exp *)

type program =
    Exp of exp
  | Decl of id * exp
  | DeclList of program * program
  | ClosedDecl of id * exp
  | ClosedLetExp of id * exp * exp
  | ClosedDeclList of program list
  | DeclListEnd of meta
  | RecDecl of id * id * exp
