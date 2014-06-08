
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


(* pretty-printing *)


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

