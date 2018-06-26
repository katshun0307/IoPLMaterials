open Syntax

exception Error of string

let err s = raise (Error s)

(* type environment *)
type tyenv = ty Environment.t
type subst = (tyvar * ty) list

let ty_prim op ty1 ty2 = match op with
  | Plus -> (match ty1, ty2 with
      | TyInt, TyInt -> TyInt
      | _ -> err "argument must be of integer: +")
  | Mult -> (match ty1, ty2 with
      | TyInt, TyInt -> TyInt
      | _ -> err "argument must be of integer: *")
  | Lt -> (match ty1, ty2 with
      | TyInt, TyInt -> TyBool
      | _ -> err "argument must be of integer: <")
  | And -> (match ty1, ty2 with
      | TyBool, TyBool -> TyBool
      | _ -> err "argument must be of boolean: &&")
  | Or -> (match ty1, ty2 with
      | TyBool, TyBool -> TyBool
      | _ -> err "argument must be of boolean: ||")

let rec ty_exp tyenv = function
  | Var x -> (try Environment.lookup x tyenv with 
        Environment.Not_bound -> err ("Variable not bound: " ^ x))
  | ILit _ -> TyInt
  | BLit _ -> TyBool
  | BinOp (op, exp1, exp2) ->
    let tyarg1 = ty_exp tyenv exp1 in
    let tyarg2 = ty_exp tyenv exp2 in
    ty_prim op tyarg1 tyarg2
  | IfExp (exp1, exp2, exp3) -> 
    let tyarg1 = ty_exp tyenv exp1 in
    (match tyarg1 with
     | TyBool -> 
       let tyarg2 = ty_exp tyenv exp2 in
       let tyarg3 = ty_exp tyenv exp3 in
       if tyarg2 = tyarg3 then tyarg2 else err("both types must be same: if")
     | _ -> err "condition must be boolean: if")
  | MultiLetExp (params, exp) -> 
    let rec extend_env_with_list current_tyenv p =
      match p with
      | (id, e) :: rest -> 
        let tyexp = ty_exp current_tyenv e in 
        let new_tyenv = Environment.extend id tyexp current_tyenv  in 
        extend_env_with_list new_tyenv rest
      | [] -> current_tyenv in
    let tyevalenv = extend_env_with_list tyenv params in
    ty_exp tyevalenv exp
  (* TODO: FunExp *)
  | _ -> err "not implemented"

let ty_decl tyenv = function
  | Exp e -> ty_exp tyenv e
  (* TODO: Decl *)
  | _ -> err "not implemented"

let rec subst_type s ty = 
  let rec resolve_subst (subst_tyvar, subst_ty) ty = 
    let subst_pair = (subst_tyvar, subst_ty) in
    match ty with
    | TyVar id -> if id = subst_tyvar then subst_ty else TyVar id
    | TyFun(a, b) -> TyFun(resolve_subst subst_pair a, resolve_subst subst_pair b)
    | TyInt -> TyInt
    | TyBool -> TyBool 
  in match s with 
  | top :: rest -> 
    subst_type rest (resolve_subst top ty)
  | [] -> ty
