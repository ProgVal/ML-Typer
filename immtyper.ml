(* TODO :
 * rewrite comments !! (a certain number of them are no longer accurate
 *)

(* typer for a simple ML-like pure functional language *)
(* immediate resolution *)

(*
 * This version of the inference algorithm rely heavily on substitution.
 * It might be quite inefficient to always apply explicitly each one of them.
 * A more optimized approach is expected to be enforced later. (cf type.ml)
 *)

open Ast
open Errors
open Type

(*
 * the typing environment that remembers
 * lexical bindings and their types
 *)
type type_env = (string * ty) list

(* apply a sunstitution over an environement *)
let subst_env s e = List.map (fun (str, ty) -> (str, s ty)) e

(*
 * Convert the result of unification (or any association list)
 * into a substitution
 *)
let alist_to_subst alist =
  (fun ty -> List.fold_left (fun res (v1, ty1) -> subst ty1 v1 ty) ty alist)
                            


let pat_infer sess pat = Typer.pat_infer sess pat

(*
 * Infers the type of an expression
 * Returns:
 *  - the inferred type, that may be a type variable
 *  - a substitution [...]
 *)
let rec infer sess env ast = match snd ast with
  | Unit  -> (TUnit, empty_subst)
  | Cst _ -> (TInt, empty_subst)
  | Var v ->
      (* typing of a variable (lookup in the type environment) *)
      (try (List.assoc v env, empty_subst)
       with Not_found ->
           span_fatal sess (fst ast) (Printf.sprintf "unbound variable %s" v))
(*
  | Let (pat, expr, body) ->
      (*
       * typing of `let x = e1 in e2' :
       *  - type x in tx
       *  - type e1 in t1
       *  - type e2 in t2, with the pattern in the environment
       *  - return t2, add t1 = tpat as constraint
       * features recursive definition by default
       *)
      let (ty_pat, nenv)      = pat_infer sess pat  in
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
      let (ty_pat, nenv) = pat_infer sess pat in
      let (ty_body, sys) = infer sess (nenv @ env) body in
      (TFunc (ty_pat, ty_body), sys)
 *)
  | If (econd, etrue, efalse) ->
     (*
      * typing of ifz e1 then e2 else e3
      *  - type of the expression is t2
      *  - add constraint t2 = t3
      *  - add constraint t1 = int
      *)
      let (ty_econd, subst0)   = infer sess env econd  in
      let subst1               =
        alist_to_subst (Unif.unify [(ty_econd, TInt)]) in
      let subst2               = (fun ty -> subst0 (
      let (ty_etrue, s_true)   = infer sess env etrue  in
      let (ty_efalse, s_false) = infer sess env efalse in
      
      let sys = sys_cond @ sys_true @ sys_false in
      (ty_etrue, (ty_econd, TInt) :: (ty_etrue, ty_efalse) :: sys)
  | _ -> failwith "TODO"
(*
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
          let (ty_pat, nenv)  = pat_infer sess pat          in
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
 *)
