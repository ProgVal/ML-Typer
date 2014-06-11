(* TEST FILE ; TO BE CLEANED IF IT WORKS *)
(* + DESCRIBE HOW DOES IT WORKS !!!!!!!! *)


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


let rec subst_ty s t = match t with
  | TUnit | TInt     -> t
  | TVar v           ->
     (try Hashtbl.find s v with Not_found -> t)
  | TTuple l         -> TTuple (List.map (subst_ty s) l)
  | TFunc (arg, res) -> TFunc (subst_ty s arg, subst_ty s res)
  | TSum l           -> TSum (List.map (fun (ctor, ty) ->
                                        (ctor, subst_ty s ty)) l)

(* apply a sunstitution over an environement *)
let subst_env s e = List.map (fun (str, ty) -> (str, subst_ty s ty)) e


(* test *)
let hashtbl_map_inplace f h =
  let keys = Hashtbl.fold (fun k _ res -> k :: res) h [] in
  List.iter (fun key -> Hashtbl.replace h key (f (Hashtbl.find h key))) keys

(* terms of l are not supposed to contain varables from s nor from l *)
(* types are not supposed to be recursive *)
let rec melt_to_subst s l = match l with
  | [] -> ()
  | (v, ty1) :: rest ->
     try let ty2 = Hashtbl.find s v in
         melt_to_subst s ((Unif.unify [(ty1, ty2)]) @ rest)
     with Not_found -> ( hashtbl_map_inplace (subst ty1 v) s ;
                         Hashtbl.add s v ty1 )



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
(* 
 * TODO : find a way to keep env up-to-date ;
 * (why not applying subst only when access ?)
 * /\ seems OK ; be careful to apply the subst when returning ...
 *)
(* TODO : handle unification errors ! *)
let infer sess env ast =
  let binds = Hashtbl.create 0 in
  let rec aux env ast = match snd ast with
    | Unit  -> TUnit
    | Cst _ -> TInt
    | Var v ->
      (* typing of a variable (lookup in the type environment) *)
      (* + update env (TODO : clean up ! *)
      (try List.assoc v (subst_env binds env)
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
      let (ty_pat, nenv) = pat_infer sess pat  in
      (* reminder : our let is reccursive by default *)
      let ty_expr = aux (nenv @ env) expr in
      (* 
       * it seems that possible variable conflicts between patttern variables
       * and the ones in binds are solved by recursively by melt_to_subst
       * so there is no need to apply the substitutions at this point
       *)
      melt_to_subst binds
                    (Unif.unify [(ty_pat, ty_expr)]) ;
      let ty_body = aux (nenv @ env) body in
      ty_body (* no need to apply subst, as by invariant, the result of the
               * function is already ... TODO : complete *)
  | Fun (pat, body) ->
      (*
       * typing of `fun x -> y' :
       *  - type x in tx
       *  - type y in ty
       * Constraints will be added on apply
       *)
      let (ty_pat, nenv) = pat_infer sess pat in
      let ty_body = aux (nenv @ env) body in
      subst_ty binds (TFunc (ty_pat, ty_body)) (* sould be no need to re-apply 
                                                * subst on ty_body ... *)
  | If (econd, etrue, efalse) ->
     (*
      * typing of ifz e1 then e2 else e3
      *  - type of the expression is t2
      *  - add constraint t2 = t3
      *  - add constraint t1 = int
      *)
      let ty_econd  = aux env econd in
      let ty_etrue  = aux env etrue in
      let ty_efalse = aux env efalse in
      melt_to_subst binds
                    (Unif.unify [(ty_econd, TInt) ; (ty_etrue, ty_efalse)]) ;
      subst_ty binds ty_etrue
  | Tuple lst ->
     (*
      * typing of a tuple
      * just collects the types and constraints of sub-expressions
      *)
     subst_ty binds (TTuple (List.map (aux env) lst))
  | BinOp (_, opl, opr) ->
     (*
      * typing of a binary operator
      *  - type of the expression is x
      *  - add the constraint that the types of operands must be int
      *)
     let ty_opl = aux env opl in
     let ty_opr = aux env opr in
     melt_to_subst binds
                    (Unif.unify [(ty_opl, TInt) ; (ty_opr, TInt)]) ;
     TInt
(*
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
 *)
  | Match _ -> failwith "TODO"
  | Apply (func, arg) ->
      (*
       * typing of a function application (f e)
       *  - type f in tf
       *  - type e in te
       *  - introduce a type tr for the result
       *  - add constraint tf = te -> tr
       *)
      let ty_arg  = aux env arg  in
      let ty_func = aux env func in
      let ty_ret  = TVar (next_var ()) in
      melt_to_subst binds
                    (Unif.unify [(TFunc (ty_arg, ty_ret), ty_func)]) ;
      subst_ty binds ty_ret
  | Ctor _ ->
      (* TODO: implement sum types (and records !) *)
      failwith "Variant types are still to be implemented"
  in (aux env ast, binds)
