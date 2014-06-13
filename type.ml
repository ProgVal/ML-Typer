
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
 * TODO: change string to a more precise type
 *)
type type_env = (string * ty) list

(* type pretty-printing *)

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
 * Substitutions as first class values
 * (used by the immediate resolution inference algorithm)
 * currently simply functions ; to be optimized
 *)
module Subst (Unif: sig val unify: (ty * ty) list -> (tvar * ty) list end) = struct
    type t = (tvar, ty) Hashtbl.t
    let empty () = Hashtbl.create 0

    (* apply the substitutions on a type *)
    let rec apply substs ty = match ty with
    | TUnit | TInt     -> ty
    | TVar v           -> (try Hashtbl.find substs v with Not_found -> ty)
    | TTuple l         -> TTuple (List.map (apply substs) l)
    | TFunc (arg, res) -> TFunc (apply substs arg, apply substs res)
    | TSum l           -> TSum (List.map (fun (ctor, ty) -> (ctor, apply substs ty)) l)

    (*
     * Merges an associative list of (variable, type) returned by the
     * unification into our subsitutions map.
     * Assumes that type terms of alist do not contain the keys of alist
     * or of the subsitutions map (this forbids recursive types)
     *)
    let rec merge substs alist = match alist with
    | [] -> ()
    | (v, ty1) :: rest ->
        try
            let ty2 = Hashtbl.find substs v in
            merge substs ((Unif.unify [(ty1, ty2)]) @ rest)
        with Not_found ->
            ( Utils.hashtbl_map_inplace (subst ty1 v) substs ;
              Hashtbl.add substs v ty1 ;
              merge substs rest )

    (* Unifies a list of equations and merge them into the substitutions *)
    let unify substs equs =
        merge substs (Unif.unify equs)
end
