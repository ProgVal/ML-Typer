(* typer for a simple ML-like pure functional language *)

open Ast
open Errors

(* This didn't change from the previous algorithm *)
let pat_infer = Typers_core.pat_infer

(*
 * infer_equss the type of an expression
 * Returns:
 *  - the infer_equsred type, that may be a type variable
 *  - an equation system with constraints on the infer_equsred types
 *)
let rec infer_equs sess env ast = match snd ast with
    | `Unit  -> (`TUnit, [])
    | `Cst _ -> (`TInt, [])

    | `Var v ->
        (* typing of a variable (lookup in the type environment) *)
        (try (List.assoc v env, [])
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
        let (ty_pat, nenv)      = pat_infer sess pat  in
        let (ty_expr, sys)      = infer_equs sess (nenv @ env) expr in
        let (ty_body, sys_body) = infer_equs sess (nenv @ env) body in
        (ty_body, (ty_expr, ty_pat) :: sys @ sys_body)

    | `Fun (pat, body) ->
        (*
         * typing of `fun x -> y' :
         *  - type x in tx
         *  - type y in ty
         * Constraints will be added on apply
         *)
        let (ty_pat, nenv) = pat_infer sess pat in
        let (ty_body, sys) = infer_equs sess (nenv @ env) body in
        (`TFunc (ty_pat, ty_body), sys)

    | `If (econd, etrue, efalse) ->
        (*
         * typing of ifz e1 then e2 else e3
         *  - type of the expression is t2
         *  - add constraint t2 = t3
         *  - add constraint t1 = int
         *)
        let (ty_econd, sys_cond)   = infer_equs sess env econd  in
        let (ty_etrue, sys_true)   = infer_equs sess env etrue  in
        let (ty_efalse, sys_false) = infer_equs sess env efalse in
        let sys = sys_cond @ sys_true @ sys_false in
        (ty_etrue, (ty_econd, `TInt) :: (ty_etrue, ty_efalse) :: sys)

    | `Tuple lst ->
        (*
         * typing of a tuple
         * just collects the types and constraints of sub-expressions
         *)
        let (types, sys_list) = List.split (List.map (infer_equs sess env) lst) in
        (`TTuple types, List.flatten sys_list)

    | `BinOp (_, opl, opr) ->
        (*
         * typing of a binary operator
         *  - type of the expression is x
         *  - add the constraint that the types of operands must be int
         *)
        let (ty_opl, sys_l) = infer_equs sess env opl in
        let (ty_opr, sys_r) = infer_equs sess env opr in
        (`TInt, (ty_opl, `TInt) :: (ty_opr, `TInt) :: sys_l @ sys_r)

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
            let (ty_pat, nenv)  = pat_infer sess pat          in
            let (ty_arm, sys)   = infer_equs sess (nenv @ env) ast in
            (ty_pat, ty_arm, sys)
        in

        let (ty_expr, sys_expr) = infer_equs sess env expr in
        let (ty_pat_car, ty_arm_car, sys_car) = type_arm car in
        let sys = List.flatten
                      (List.map
                          (fun arm ->
                              let (ty_pat, ty_arm, sys) = type_arm arm in
                              (ty_pat_car, ty_pat) :: (ty_arm_car, ty_arm) :: sys)
                      cdr)
        in
        (ty_arm_car, (ty_pat_car, ty_expr) :: sys)

    | `Match (_, []) -> failwith "ICE : empty patterns are not supposed to be."

    | `Apply (func, arg) ->
        (*
         * typing of a function application (f e)
         *  - type f in tf
         *  - type e in te
         *  - introduce a type tr for the result
         *  - add constraint tf = te -> tr
         *)
        let (ty_func, sys_func) = infer_equs sess env func in
        let (ty_arg, sys_arg)   = infer_equs sess env arg  in
        let ty_ret = `TVar (Types.next_var ()) in
        (ty_ret, (`TFunc (ty_arg, ty_ret), ty_func) :: sys_func @ sys_arg)

let infer sess env ast =
    let (ty, equs) = infer_equs sess env ast in
    let alist = Types.Unif.unify equs in
    let substs = Types.Subst.from_alist alist in
    Types.Subst.apply substs ty
