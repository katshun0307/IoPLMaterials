open Syntax
open MySet

exception Error of string

let err s = raise (Error s)

(* type environment *)
type tyenv = ty Environment.t
type subst = (tyvar * ty) list

(* resolved type from unified result*)
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

(* unify(solve) type constraints *)
let rec unify lst_to_unify  = 
  let rec loop lst current_subst = 
    (match lst with
     | (x, y) :: rest -> 
       if x = y then loop rest current_subst else
         (match x, y with
          | TyFun(a, b), TyFun(c, d) -> loop rest (loop [(a, c); (b, d)] current_subst)
          | TyVar(id), b -> 
            if not (MySet.member (TyVar id) (freevar_ty b)) then (loop rest ((id, b) :: current_subst)) 
            else err "unify: could not resolve type"
          | b, TyVar(id) -> 
            if not (MySet.member (TyVar id) (freevar_ty b)) then (loop rest ((id, b) :: current_subst)) 
            else err "unify: could not resolve type"
          | _ -> err "unify: could not resolve type"
         )
     | _ -> current_subst) in 
  loop lst_to_unify []

let ty_prim op ty1 ty2 tycons = match op with
  | Plus -> (match ty1, ty2 with
      | TyInt, TyInt -> (TyInt, tycons)
      | TyVar id, TyInt -> (TyInt, MySet.insert (TyVar id, TyInt) tycons) 
      | TyInt, TyVar id -> (TyInt, MySet.insert (TyVar id, TyInt) tycons) 
      | TyVar id1, TyVar id2 -> (TyInt, MySet.insert (TyVar id1, TyInt) (MySet.insert (TyVar id2, TyInt) tycons))
      | _ -> err "argument must be of integer: +")
  | Mult -> (match ty1, ty2 with
      | TyInt, TyInt -> (TyInt, tycons)
      | TyVar id, TyInt -> (TyInt, MySet.insert (TyVar id, TyInt) tycons) 
      | TyInt, TyVar id -> (TyInt, MySet.insert (TyVar id, TyInt) tycons)
      | TyVar id1, TyVar id2 -> (TyInt, MySet.insert (TyVar id1, TyInt) (MySet.insert (TyVar id2, TyInt) tycons))
      | _ -> err "argument must be of integer: *")
  | Lt -> (match ty1, ty2 with
      | TyInt, TyInt -> (TyBool, tycons)
      | TyVar id, TyInt -> (TyBool, MySet.insert (TyVar id, TyInt) tycons) 
      | TyInt, TyVar id -> (TyBool, MySet.insert (TyVar id, TyInt) tycons) 
      | TyVar id1, TyVar id2 -> (TyBool, MySet.insert (TyVar id1, TyInt) (MySet.insert (TyVar id2, TyInt) tycons))
      | _ -> err "argument must be of integer: <")

let ty_logic op ty1 ty2 tycons = 
  match op with
  | And -> (match ty1, ty2 with
      | TyBool, TyBool -> (TyBool, tycons)
      | TyVar id, TyBool -> (TyBool, MySet.insert (TyVar id, TyBool) tycons)
      | TyBool, TyVar id -> (TyBool, MySet.insert (TyVar id, TyBool) tycons)
      | TyVar id1, TyVar id2 -> (TyBool, MySet.insert (TyVar id1, TyBool) (MySet.insert (TyVar id2, TyBool) tycons))
      | _ -> err "argument must be of boolean: &&")
  | Or -> (match ty1, ty2 with
      | TyBool, TyBool -> (TyBool, tycons)
      | TyVar id, TyBool -> (TyBool, MySet.insert (TyVar id, TyBool) tycons)
      | TyBool, TyVar id -> (TyBool, MySet.insert (TyVar id, TyBool) tycons)
      | TyVar id1, TyVar id2 -> (TyBool, MySet.insert (TyVar id1, TyBool) (MySet.insert (TyVar id2, TyBool) tycons))
      | _ -> err "argument must be of boolean: ||")

let get_type = function
  | TyVar _ -> "tyvar"
  | TyBool -> "tybool"
  | TyInt -> "tyint"
  | TyFun _ -> "tyfun"    

let rec ty_exp tyenv tycons = function
  | Var x -> 
    (try (Environment.lookup x tyenv, tycons) with 
       Environment.Not_bound -> err ("Variable not bound: " ^ x))
  | ILit _ -> (TyInt, tycons)
  | BLit _ -> (TyBool, tycons)
  | BinOp (op, exp1, exp2) -> 
    (let tyarg1, tycons1 = ty_exp tyenv tycons exp1 in
     let tyarg2, tycons2 = ty_exp tyenv tycons exp2 in
     ty_prim op tyarg1 tyarg2 (MySet.union tycons1 tycons2))
  | LogicOp(op, exp1, exp2) -> 
    (let tyarg1, tycons1 = ty_exp tyenv tycons exp1 in
     let tyarg2, tycons2 = ty_exp tyenv tycons exp2 in
     ty_logic op tyarg1 tyarg2 (MySet.union tycons1 tycons2))
  | IfExp (exp1, exp2, exp3) -> 
    let tyarg1, tycons1 = ty_exp tyenv tycons exp1 in
    let cond_type = get_type tyarg1 in
    (* if condition part is valid *)
    if cond_type = "tybool" || cond_type = "tyvar" then
      let new_cons = MySet.insert (tyarg1, TyBool) tycons1 in
      let tyarg2, tycons2 = ty_exp tyenv new_cons exp2 in
      let tyarg3, tycons3 = ty_exp tyenv new_cons exp3 in
      (* if tyarg1 then tyatg2 else tyarg3 *)
      if tyarg2 = tyarg3 then (* both types are determined *)
        (tyarg3, MySet.union tycons2 tycons3)
      else
        (match tyarg2, tyarg3 with
         | TyVar id1, TyVar id2 ->
           (* enters here *)
           (tyarg2, MySet.insert (tyarg2, tyarg3) (MySet.union tycons2 tycons3))
         | TyVar id, known_ty -> 
           (known_ty, MySet.insert (TyVar id, known_ty) (MySet.union tycons2 tycons3))
         | known_ty, TyVar id -> 
           (known_ty, MySet.insert (TyVar id, known_ty) (MySet.union tycons2 tycons3))
         | _ -> err "both types doesn't match: if")
    else err "condition must be boolean: if"
  | MultiLetExp (params, exp) -> 
    let rec extend_envs_from_list current_tyenv current_cons p =
      match p with
      | (id, e) :: rest -> 
        let e_type, e_cons = ty_exp tyenv tycons e in
        if get_type e_type = "tyvar" then err "unknown variable: multiletexp"
        else let new_tyenv = Environment.extend id e_type current_tyenv in
          extend_envs_from_list new_tyenv current_cons rest
      | [] -> (current_tyenv, current_cons) in
    let eval_tyenv, eval_cons = extend_envs_from_list tyenv tycons params in
    ty_exp eval_tyenv eval_cons exp
  | FunExp(params, exp) -> 
    let rec extend_envs_from_list current_env p = 
      (match p with
       | id :: rest -> 
         let new_tyvar = TyVar (fresh_tyvar()) in 
         let new_env = Environment.extend id new_tyvar current_env in
         extend_envs_from_list new_env rest
       | _ -> current_env ) in 
    (* get environment with new tyvar for each params to evaluate the main function (tycons has no meaning) *)
    let eval_tyenv = extend_envs_from_list tyenv params in
    (* evaluate main function in the created environment *)
    let res_ty, res_tycons = ty_exp eval_tyenv tycons exp in
    (* make output ( re-evaluate args ) *)
    let unified = unify (MySet.to_list res_tycons) in
    let rec eval_type p e = 
      match p with
      | top :: rest -> 
        let arg_tyvar = Environment.lookup top eval_tyenv in
        let arg_ty = subst_type unified arg_tyvar in
        TyFun(arg_ty, eval_type rest e)
      | [] -> e in
    (eval_type params res_ty, res_tycons)
  | AppExp(exp1, exp2) ->
    let ty_exp1, cons_exp1 = ty_exp tyenv tycons exp1 in
    let ty_exp2, cons_exp2 = ty_exp tyenv tycons exp2 in
    let app_cons = MySet.union cons_exp1 cons_exp2 in
    (match ty_exp1 with 
     | TyFun(ty1, ty2) -> 
       (ty2, MySet.insert (ty1, ty_exp2) app_cons)
     | TyVar n -> 
       let ty2 = TyVar (fresh_tyvar ()) in
       (ty2, (MySet.insert (TyVar n, TyFun(ty_exp2, ty2)) app_cons))
     | _ -> err "application to non-function")
  | _ -> err "ty_exp: not implemented"

let rec ty_decl tyenv = function
  | Exp e -> 
    let (type_, cons) = ty_exp tyenv MySet.empty e in
    let unified = unify (MySet.to_list cons) in
    let resolved_type = subst_type unified type_ in 
    (resolved_type, tyenv)
  | Decl(id, e) -> 
    let e_ty, unified = ty_decl tyenv (Exp e) in
    let new_tyenv = Environment.extend id e_ty tyenv in
    (e_ty, new_tyenv)

