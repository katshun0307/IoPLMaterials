open Syntax
open MySet

exception Error of string

let err s = raise (Error s)

(* type environment *)
type tyenv = ty Environment.t
type subst = (tyvar * ty) list

(* apply subst:(substutution) to ty:(type) *)
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

(* reform subst(substitution) to eql:(list of equal types) *)
let eqls_of_subst subst =  
  let reform sub eq = 
    let ((id: tyvar), (t: ty)) = sub in 
    (TyVar id, t) in
  List.map reform subst

(* apply subst:(substitution) to eql:(list of equal types) *)
let subst_eqs subst eql = 
  List.map (fun (t1, t2) -> (subst_type subst t1, subst_type subst t2)) eql

(* let rec compose_subst subst1 subst2 =
   let subst2_ = List.map (fun (tx ,t) -> (tx, subst_type theta1 t)) theta2  
   in List.fold_left (fun tau -> fun (tx, t) -> 
      try 
        let  _ = ) *)

(* unify(solve) type constraints *)
(* let rec replace_list l before after = 
   let rec replace s t ty =
    match ty with
    | TyFun(a, b) -> if ty = s then t else TyFun(replace a s t, replace b s t)
    | _ -> if ty = s then t else ty in
   let replace_pair s t p = 
    match p with 
    | (a, b) -> (replace s t a, replace s t b) in
   List.map (replace_pair before after) l

   let id_of_tyvar = function
    | TyVar id -> id
    | _ -> err "this is not a TyVar: id_of_tyvar" *)

let rec unify eqs  = 
  let rec loop lst current_subst = 
    (match lst with
     | (x, y) :: rest -> 
       if x = y then loop rest current_subst else
         (match x, y with
          | TyFun(a, b), TyFun(c, d) -> loop ((a, c) :: (b, d) :: rest) current_subst
          | TyVar(id), b -> 
            if not (MySet.member (TyVar id) (freevar_ty b)) then 
              loop (subst_eq [(id, b)] rest) 
            else err "unify: could not resolve type"
          | b, TyVar(id) -> 
            if not (MySet.member (TyVar id) (freevar_ty b)) then
              let unified = unify (replace_list rest x y) in
              replace_list unified x y
            else err "unify: could not resolve type"
          | _ -> err "unify: could not resolve type"
         )
     | _ -> current_subst) in 
  loop lst_to_unify []

let resolve cons ty = 
  let clean p = 
    (match p with
     | (TyVar id, other) -> (id, other)
     | (other, TyVar id) -> (id, other)
     | _ -> err "unexpected") in
  subst_type (List.map clean (unify (MySet.to_list cons))) ty

let ty_prim op ty1 ty2 tycons = match op with
  | Plus -> (TyInt, MySet.insert (ty1, TyInt) (MySet.insert (ty2, TyInt) tycons))
  | Mult -> (TyInt, MySet.insert (ty1, TyInt) (MySet.insert (ty2, TyInt) tycons))
  | Lt ->  (TyBool, MySet.insert (ty1, TyInt) (MySet.insert (ty2, TyInt) tycons))

let ty_logic op ty1 ty2 tycons = 
  match op with
  | And -> (TyBool, MySet.insert (ty1, TyBool) (MySet.insert (ty2, TyBool) tycons))
  | Or  -> (TyBool, MySet.insert (ty1, TyBool) (MySet.insert (ty2, TyBool) tycons))

let get_type = function
  | TyVar _ -> "tyvar"
  | TyBool -> "tybool"
  | TyInt -> "tyint"
  | TyFun _ -> "tyfun"    

let rec ty_exp tyenv tycons = function
  | Var x -> 
    (try (resolve tycons (Environment.lookup x tyenv), tycons) with 
       Environment.Not_bound -> err ("Variable not bound: " ^ x))
  | ILit _ -> (TyInt, tycons)
  | BLit _ -> (TyBool, tycons)
  | BinOp (op, exp1, exp2) -> 
    (let tyarg1, tycons1 = ty_exp tyenv tycons exp1 in
     let tyarg2, tycons2 = ty_exp tyenv tycons exp2 in
     let res_ty, res_tycons = ty_prim op tyarg1 tyarg2 (MySet.union tycons1 tycons2) in
     (resolve res_tycons res_ty, res_tycons))
  | LogicOp(op, exp1, exp2) -> 
    (let tyarg1, tycons1 = ty_exp tyenv tycons exp1 in
     let tyarg2, tycons2 = ty_exp tyenv tycons exp2 in
     let res_ty, res_tycons = ty_logic op tyarg1 tyarg2 (MySet.union tycons1 tycons2) in
     (resolve res_tycons res_ty, res_tycons))
  | IfExp (exp1, exp2, exp3) -> 
    let tyarg1, tycons1 = ty_exp tyenv tycons exp1 in
    let cond_type = get_type tyarg1 in
    (* if condition part is valid *)
    if cond_type = "tybool" || cond_type = "tyvar" then
      let new_cons = MySet.insert (tyarg1, TyBool) tycons1 in
      let tyarg2, tycons2 = ty_exp tyenv new_cons exp2 in
      let tyarg3, tycons3 = ty_exp tyenv new_cons exp3 in
      let res_cons = MySet.union tycons2 tycons3 in
      (resolve (MySet.insert (tyarg2, tyarg3) res_cons) tyarg2, MySet.insert (tyarg2, tyarg3) res_cons)
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
    (* let unified = unify (MySet.to_list res_tycons) in *)
    let rec eval_type p e = 
      match p with
      | top :: rest -> 
        let arg_tyvar = Environment.lookup top eval_tyenv in
        let arg_ty =  resolve res_tycons arg_tyvar in
        TyFun(arg_ty, eval_type rest e)
      | [] -> e in
    (eval_type params res_ty, res_tycons)
  | AppExp(exp1, exp2) ->
    let ty_exp1, cons_exp1 = ty_exp tyenv tycons exp1 in
    let ty_exp2, cons_exp2 = ty_exp tyenv tycons exp2 in
    let app_cons = MySet.union cons_exp1 cons_exp2 in
    (match ty_exp1 with 
     | TyFun(ty1, ty2) -> 
       let eval_cons = MySet.insert (ty1, ty_exp2) app_cons in
       (resolve eval_cons ty2, eval_cons)
     | TyVar n -> 
       let ty2 = TyVar (fresh_tyvar ()) in
       let eval_cons = MySet.insert (TyVar n, TyFun(ty_exp2, ty2)) app_cons in
       (resolve eval_cons ty2, eval_cons)
     | _ -> err "application to non-function")
  | _ -> err "ty_exp: not implemented"

let rec ty_decl tyenv = function
  | Exp e -> 
    let (type_, cons) = ty_exp tyenv MySet.empty e in
    (* let unified = unify (MySet.to_list cons) in *)
    let resolved_type = resolve cons type_ in 
    (resolved_type, tyenv)
  | Decl(id, e) -> 
    let e_ty, unified = ty_decl tyenv (Exp e) in
    let new_tyenv = Environment.extend id e_ty tyenv in
    (e_ty, new_tyenv)
