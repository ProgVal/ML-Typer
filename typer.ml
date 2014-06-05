(* typer for a simple ML-like pure functional language *)

open Ast
open Errors

(* a type variable *)
type tvar = int

(* generates a unique type var *)
let next_var =
    let n = ref 0 in
    (fun () ->
        let ret = !n in
        n := !n + 1 ;
        ret)

(* 
 * the type of an expression may be either
 * a type variable or a concrete type
 *)
type ty =
  | TUnit
  | TInt
  | TVar   of tvar
  | TTuple of ty list
  | TFunc  of ty * ty
  | TSum   of ctor list
and ctor = (string * ty)

(*
 * the typing environment that remembers
 * lexical bindings and their types
 *)
type type_env = (string * ty) list

(*
 * equation system that stores all the constraints
 * encountered during type inference
 *)
type equ = (ty * ty)
type equ_sys = equ list

(*
 * Type the pattern of a let form or a function argument
 * Returns:
 *  - the type of the whole pattern
 *  - a type environment for bindings defined in the pattern
 *)
let rec pat_infer pat = match snd pat with
  | PatUnit  -> (TUnit, [])
  | PatCst _ -> (TInt, [])
  | PatWildcard ->
     let ty = TVar (next_var ()) in
     (ty, [])
  | PatVar v ->
      let ty = TVar (next_var ()) in
      (ty, [(v, ty)])
  | PatCtor (name, pat) ->
      let (pat_ty, pat_env) = pat_infer pat in
      (TSum [(name, pat_ty)], pat_env)
  | PatTup pats ->
      let (types, env_list) = List.split (List.map pat_infer pats) in
      (TTuple types, List.flatten env_list)

(*
 * Infers the type of an expression
 * Returns:
 *  - the inferred type, that may be a type variable
 *  - an equation system with constraints on the inferred types
 *)
let rec infer sess env ast = match snd ast with
  | Unit  -> (TUnit, [])
  | Cst _ -> (TInt, [])
  | Var v ->
      (* typing of a variable (lookup in the type environment) *)
      (try (List.assoc v env, [])
       with Not_found ->
           span_fatal sess (fst ast) (Printf.sprintf "unbound variable %s" v))
  | Let (pat, expr, body) ->
      (*
       * typing of `let x = e1 in e2' :
       *  - type x in tx
       *  - type e1 in t1
       *  - type e2 in t2, with the pattern in the environment
       *  - return t2, add t1 = tpat as constraint
       * features recursive definition by default
       *)
      let (ty_pat, nenv)      = pat_infer pat  in
      (* [t] : I might not be completely sure, but I think the change
      let (ty_expr, sys)      = infer env expr in
      to
      let (ty_expr, sys)      = infer (nenv @ env) expr in
      is sufficient to support recursive definition (at least in the infer)
      the rest should be handled by the unification *)
      let (ty_expr, sys)      = infer sess (nenv @ env) expr in
      let (ty_body, sys_body) = infer sess (nenv @ env) body in
      (ty_body, (ty_expr, ty_pat) :: sys @ sys_body)
  | Fun (pat, body) ->
      (*
       * typing of `fun x -> y' :
       *  - type x in tx
       *  - type y in ty
       * Constraints will be added on apply
       *)
      let (ty_pat, nenv) = pat_infer pat in
      let (ty_body, sys) = infer sess (nenv @ env) body in
      (TFunc (ty_pat, ty_body), sys)
  | If (econd, etrue, efalse) ->
      (*
       * typing of ifz e1 then e2 else e3
       *  - type of the expression is t2
       *  - add constraint t2 = t3
       *  - add constraint t1 = int
       *)
      let (ty_econd, sys_cond)   = infer sess env econd  in
      let (ty_etrue, sys_true)   = infer sess env etrue  in
      let (ty_efalse, sys_false) = infer sess env efalse in
      let sys = sys_cond @ sys_true @ sys_false in
      (ty_etrue, (ty_econd, TInt) :: (ty_etrue, ty_efalse) :: sys)
  | Tuple lst ->
      (*
       * typing of a tuple
       * just collects the types and constraints of sub-expressions
       *)
      let (types, sys_list) = List.split (List.map (infer sess env) lst) in
      (TTuple types, List.flatten sys_list)
  | BinOp (_, opl, opr) ->
      (*
       * typing of a binary operator
       *  - type of the expression is x
       *  - add the constraint that the types of operands must be int
       *)
      let (ty_opl, sys_l) = infer sess env opl in
      let (ty_opr, sys_r) = infer sess env opr in
      (TInt, (ty_opl, TInt) :: (ty_opr, TInt) :: sys_l @ sys_r)
  | Match (expr, (car :: cdr)) ->
      (*
       * typing of a match expr with ...
       *  - type expr into ty_expr
       *  - type each arm t into ty_t with the pattern in the environment
       *  - add the constraint that ty_0 = ty_1 ... = ty_n
       *  - add the constraint that each pattern has the same type
       *  - add the constraint that ty_expr = type of the patterns
       *  - the final type is ty_0
       *)
      let type_arm (pat, ast) =
          let (ty_pat, nenv)  = pat_infer pat          in
          let (ty_arm, sys)   = infer sess (nenv @ env) ast in
          (ty_pat, ty_arm, sys) in
      let (ty_expr, sys_expr) = infer sess env expr in
      let (ty_pat_car, ty_arm_car, sys_car) = type_arm car in
      let sys = List.flatten (List.map (fun arm ->
          let (ty_pat, ty_arm, sys) = type_arm arm in
          (ty_pat_car, ty_pat) :: (ty_arm_car, ty_arm) :: sys) cdr) in
      (ty_arm_car, (ty_pat_car, ty_expr) :: sys)
  | Match (_, []) -> failwith "ICE : empty patterns are not supposed to be."
  | Apply (func, arg) ->
      (*
       * typing of a function application (f e)
       *  - type f in tf
       *  - type e in te
       *  - introduce a type tr for the result
       *  - add constraint tf = te -> tr
       *)
      let (ty_func, sys_func) = infer sess env func in
      let (ty_arg, sys_arg)   = infer sess env arg  in
      let ty_ret = TVar (next_var ()) in
      (ty_ret, (TFunc (ty_arg, ty_ret), ty_func) :: sys_func @ sys_arg)
  | Ctor _ ->
      (* TODO: implement sum types (and records !) *)
      failwith "Variant types are still to be implemented"

let rec ty_to_string ty = match ty with
  | TUnit                    -> "*unit*" ;
  | TInt                     -> "int" ;
  | TVar v                   -> Printf.sprintf "a%d" v
  | TTuple []                -> failwith "empty tuple"
  | TTuple (car :: cdr)      -> Printf.sprintf
                                    "(%s)"
                                    (List.fold_left
                                        (fun res ty -> (Printf.sprintf
                                                           "%s, %s" res
                                                           (ty_to_string ty)))
                                        (ty_to_string car) cdr)
  | TFunc (arg, res)         -> Printf.sprintf "%s -> %s" (ty_to_string arg)
                                                  (ty_to_string res)
  | TSum []                  -> failwith "empty sum type"
  | TSum ((ctor, ty) :: cdr) -> Printf.sprintf
                                    "(%s)"
                                    (List.fold_left
                                        (fun res (ctor, ty) ->
                                            (Printf.sprintf "%s | Ctor%s %s"
                                                            res ctor
                                                            (ty_to_string ty)))
                                        (Printf.sprintf "Ctor%s %s" ctor
                                                        (ty_to_string ty)) cdr)


(*
 * Unification
 *)

(*
 * Be careful with the built-in equality especially over type terms and
 * type variables ; it will work for the moment, as it is structural,
 * but if we change (e.g. for optimisation purpose) something in the
 * representation, it may not work correctly anymore.
 *)


(*
 * Substitution of a type term to a given type variable inside one type term
 * (substitution is quite trivial as there are no cature problems)
 *)
let rec subst t1 x1 t = match t with
  | TUnit | TInt     -> t
  | TVar v           -> if v = x1 then t1 else t
  | TTuple l         -> TTuple (List.map (subst t1 x1) l)
  | TFunc (arg, res) -> TFunc (subst t1 x1 arg, subst t1 x1 res)
  | TSum l           -> TSum (List.map (fun (ctor, ty) ->
                                        (ctor, subst t1 x1 ty)) l)

(*
 * Check of a type variable occurence iside a type term
 *)
let rec occur_check x t = match t with
  | TUnit | TInt     -> false
  | TVar v           -> (v = x)
  | TTuple l         -> List.exists (occur_check x) l
  | TFunc (arg, res) -> occur_check x arg || occur_check x res
  | TSum l           -> List.exists (fun (_, ty) -> occur_check x ty) l
                                     

(*
 * Exception raised whenever the unification is impossible
 * (maybe we should prefer option type, as it guarantees things through the
 * ocaml type system, while exception are not guarated to be caught)
 * TODO : have a more detailed failure system (implies to modify parsing...) !!
 *)
exception ImpossibleToUnify


(*
 * One of the main function of unification.
 * TODO : more explanations !!
 *)
let rec get_var_eq se = match se with
  | [] -> None
  | eq :: rest -> (
    match eq with
    (* same varables ; remove the equation *)
    | (TVar v1, TVar v2) when v1 = v2  -> get_var_eq rest
    (* variable to be substituted *)
    | (TVar v, t) | (t, TVar v)        ->
      if occur_check v t then failwith "Recursive types are not allowed yet"
      else Some ((v, t), rest)
    (*
     * constants
     * must be put after the variables, otherwise, the _ would match it
     *)
    | (TUnit, TUnit)                           -> get_var_eq rest
    | (TUnit, _)                               -> raise ImpossibleToUnify
    | (TInt, TInt)                             -> get_var_eq rest
    | (TInt, _)                                -> raise ImpossibleToUnify
    | (TSum l1, TSum l2)                       ->
       failwith
         "Unification of polymorphic variant types is not yet implemented"
    | (TSum _, _)                              -> raise ImpossibleToUnify
    (*
     * Splitting structures. Be careful with function when there will be
     * subtypes.
     *)
    | (TTuple l1, TTuple l2)                   ->
       ( try
           get_var_eq (List.rev_append (List.combine l1 l2) rest)
          with Invalid_argument "List.combine" -> raise ImpossibleToUnify
         )
    | (TTuple _, _)                            -> raise ImpossibleToUnify
    | (TFunc (arg1, res1), TFunc (arg2, res2)) ->
       get_var_eq ((arg1, arg2)::(res1, res2)::rest)
    | (TFunc _, _)                             -> raise ImpossibleToUnify
  )

(* Unify function. Takes a system of equations and return an association table
 * that map each variable in the system to a type term.
 * Rely heavily on the above function (notice that both are tail-recursive).
 * TODO : ensure that there aren't any free type variable
 * (witch are NOT to be confused with polymorphic types)
 *)
let unify se =
  let rec aux solved current = match get_var_eq current with
  | None -> solved
  | Some ((v, ty), sys) ->
     aux ((v, ty)::(List.map (fun (x, t) -> (x, subst ty v t)) solved))
         (List.map (fun (t1, t2) -> (subst ty v t1, subst ty v t2)) sys)
  in aux [] se
