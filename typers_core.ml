(* typer for a simple ML-like pure functional language *)

open Ast
open Errors
open Types

(*
 * Type the pattern of a let form or a function argument
 * Returns:
 *  - the type of the whole pattern
 *  - a type environment for bindings defined in the pattern
 * TODO: this version of pat_typer is more generic than needed
 * for this specific typer as it handles variant types. It should
 * be moved elsewhere.
 *)
let pat_infer sess pat =
    let h = Hashtbl.create 0 in
    let rec aux pat = match snd pat with
        | `PatUnit  -> (`TUnit, [])
        | `PatCst _ -> (`TInt, [])

        | `PatWildcard ->
            let ty = `TVar (next_var ()) in (ty, [])

        | `PatVar v ->
            if Hashtbl.mem h v then
                (* shouldn't the error message highlight the whole pattern ? *)
                span_fatal sess (fst pat)
                           (Printf.sprintf "Variable %s is bound several \
                                            times in this pattern" v)
            else (
                Hashtbl.add h v () ;
                let ty = `TVar (next_var ()) in
                (ty, [(v, ty)])
            )

        | `PatCtor (name, pat) ->
            let (pat_ty, pat_env) = aux pat in
            (`TSum [(name, pat_ty)], pat_env)

        | `PatTup pats ->
            let (types, env_list) = List.split (List.map aux pats) in
            (`TTuple types, List.flatten env_list)

    in aux pat

(*
 * Infers the type of an expression
 * Returns:
 *  - the inferred type, that may be a type variable
 *  - a map of substitutions
 * TODO : handle unification errors !
 *)
let infer sess env ast =
    let bindings = Subst.empty () in
    let rec aux env ast = match snd ast with
        | `Unit  -> `TUnit
        | `Cst _ -> `TInt

        | `Var v ->
            (* typing of a variable (lookup in the type environment) *)
            (* + update env (TODO: lazy substitutions! *)
            (try Subst.apply bindings (List.assoc v env)
             with Not_found ->
                 span_fatal sess (fst ast) (Printf.sprintf "unbound variable %s" v))

        | `Let (pat, expr, body) ->
            (*
            * typing of `let x = e1 in e2' :
            *  - type x in tx
            *  - type e1 in t1
            *  - type e2 in t2, with the pattern in the environment
            *  - return t2, add t1 = tpat as constraint
            * features recursive definition by default
            *)
            let (ty_pat, nenv) = pat_infer sess pat    in

            (*
             * the change from:
             *   let (ty_expr, sys)      = infer env expr in
             * to:
             *   let (ty_expr, sys)      = infer (nenv @ env) expr in
             * is sufficient to support recursive definition for now
             * the rest should be handled by the unification
             *)
            let ty_expr        = aux (nenv @ env) expr in
            Subst.unify bindings [(ty_pat, ty_expr)] ;
            let ty_body = aux (nenv @ env) body in
            Subst.apply bindings ty_body

        | `Fun (pat, body) ->
            (*
             * typing of `fun x -> y' :
             *  - type x in tx
             *  - type y in ty
             * Constraints will be added on apply
             *)
            let (ty_pat, nenv) = pat_infer sess pat    in
            let ty_body        = aux (nenv @ env) body in
            Subst.apply bindings (`TFunc (ty_pat, ty_body))

        | `If (econd, etrue, efalse) ->
            (*
             * typing of ifz e1 then e2 else e3
             *  - type of the expression is t2
             *  - add constraint t2 = t3
             *  - add constraint t1 = int
             *)
            let ty_econd  = aux env econd  in
            let ty_etrue  = aux env etrue  in
            let ty_efalse = aux env efalse in
            Subst.unify bindings [(ty_econd, `TInt) ; (ty_etrue, ty_efalse)] ;
            Subst.apply bindings ty_etrue

        | `Tuple lst ->
            (*
             * typing of a tuple
             * just collects the types and constraints of sub-expressions
             *)
            Subst.apply bindings (`TTuple (List.map (aux env) lst))

        | `BinOp (_, opl, opr) ->
            (*
             * typing of a binary operator
             *  - type of the expression is x
             *  - add the constraint that the types of operands must be int
             *)
            let ty_opl = aux env opl in
            let ty_opr = aux env opr in
            Subst.unify bindings [(ty_opl, `TInt) ; (ty_opr, `TInt)] ;
            `TInt

        | `Match (expr, (car :: cdr)) ->
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
                let (ty_pat, nenv)  = pat_infer sess pat   in
                let ty_arm          = aux (nenv @ env) ast in
                (ty_pat, ty_arm)
            in

            let ty_expr                  = aux env expr in
            let (ty_pat_car, ty_arm_car) = type_arm car in
            let sys = List.flatten
                          (List.map
                              (fun arm ->
                                  let (ty_pat, ty_arm) = type_arm arm in
                                  [(ty_pat_car, ty_pat); (ty_arm_car, ty_arm)])
                              cdr) in
            Subst.unify bindings ((ty_pat_car, ty_expr) :: sys) ;
            Subst.apply bindings ty_arm_car

        | `Match (_, []) -> failwith "ICE : empty patterns are not supposed to be."

        | `Apply (func, arg) ->
            (*
             * typing of a function application (f e)
             *  - type f in tf
             *  - type e in te
             *  - introduce a type tr for the result
             *  - add constraint tf = te -> tr
             *)
            let ty_arg  = aux env arg  in
            let ty_func = aux env func in
            let ty_ret  = `TVar (next_var ()) in
            Subst.unify bindings [(`TFunc (ty_arg, ty_ret), ty_func)] ;
            Subst.apply bindings ty_ret

    in aux env ast
