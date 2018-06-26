open MySet

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

type tyvar = int

type ty = 
  | TyInt 
  | TyBool
  | TyVar of tyvar
  | TyFun of ty * ty

let rec pp_ty = function
  | TyInt -> print_string "int"
  | TyBool -> print_string "bool"
  | TyVar id -> print_string ("tyvar: " ^ string_of_int id)
  | TyFun(a, b)-> 
    (pp_ty a;
     print_string " -> ";
     pp_ty b;)

(* returns new type variables with fresh_tyvar() *)
let fresh_tyvar =
  let counter = ref 0 in
  let body () =
    let v = !counter in
    counter := v + 1; v
  in body

(* 与えられた型中の型変数の集合を返す関数 *)
let rec freevar_ty ty = 
  match ty with
  | TyVar a -> MySet.from_list (TyVar a :: [])
  | TyFun(a, b) -> MySet.union (freevar_ty a) (freevar_ty b)
  | _ -> MySet.empty

