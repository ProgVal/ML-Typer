(* Internal representation of types *)

(* a type variable *)
type tvar = int

(* generates a unique type var *)
let next_var: unit -> tvar =
    let n = ref 0 in
    (fun () ->
        let ret = !n in
        n := !n + 1 ;
        ret)

(* 
 * the type of an expression may be either
 * a type variable or a concrete type
 *)
type ty = [
    | `TUnit
    | `TInt
    | `TVar   of tvar
    | `TTuple of ty list
    | `TFunc  of ty * ty
    | `TSum   of ctor list
] and ctor = (string * ty)

let open_ty (ty: [< ty]): [> ty] = match ty with
    | _ as ty1 -> ty1

(*
 * the typing environment that remembers
 * lexical bindings and their types
 * TODO: change string to a more precise type
 *)
type type_env = (string * ty) list

(* type pretty-printing *)

let rec ty_to_string (ty: [< ty]) = match ty with
    | `TUnit                    -> "*unit*" ;
    | `TInt                     -> "int" ;
    | `TVar v                   -> Printf.sprintf "a%d" v
    | `TTuple []                -> failwith "empty tuple"

    | `TTuple (car :: cdr)      ->
        Printf.sprintf "(%s)" (List.fold_left
                                  (fun res ty -> (Printf.sprintf
                                                     "%s, %s" res
                                                     (ty_to_string ty)))
                                  (ty_to_string car) cdr)

    | `TFunc (arg, res)         ->
          Printf.sprintf "%s -> %s" (ty_to_string arg) (ty_to_string res)

    | `TSum []                  -> failwith "empty sum type"

    | `TSum ((ctor, ty) :: cdr) ->
        Printf.sprintf "(%s)" (List.fold_left
                                  (fun res (ctor, ty) ->
                                      (Printf.sprintf "%s | Ctor%s %s"
                                                      res ctor
                                                      (ty_to_string ty)))
                                  (Printf.sprintf "Ctor%s %s" ctor
                                                  (ty_to_string ty)) cdr)

let rec sty_to_string sty = match sty with
    | `TSTy t          -> Printf.sprintf "[%s]" (ty_to_string t)
    | `TSForall (v, t) -> Printf.sprintf "∀a%d.%s" v (sty_to_string t)

(*
 * Substitution of a type term to a given type variable inside one type term
 * (substitution is quite trivial as there are no cature problems)
 *)
let rec subst t1 x1 (t: [< ty]) = match t with
    | `TUnit | `TInt    -> t
    | `TVar v           -> if v = x1 then t1 else t
    | `TTuple l         -> `TTuple (List.map (subst t1 x1) l)
    | `TFunc (arg, res) -> `TFunc (subst t1 x1 arg, subst t1 x1 res)
    | `TSum l           -> `TSum (List.map (fun (c, ty) -> (c, subst t1 x1 ty)) l)

(* types and functions for polymorphic typing *)

(*
 * a type scheme, which is either:
 *  - a concrete type
 *  - a type scheme with quantified variables
 *)
type ty_sch = [
    | `TSTy     of ty
    | `TSForall of tvar * ty_sch
]

(* a set of type variables *)
module VarSet = Set.Make (struct
                              type t = tvar
                              let compare = Pervasives.compare
                          end)

(* returns the set of type variables that appear in a type *)
let rec ty_vars (ty: [< ty]) = match ty with
    | `TUnit  -> VarSet.empty
    | `TInt   -> VarSet.empty
    | `TVar v -> VarSet.singleton v

    | `TTuple lst ->
        List.fold_left (fun acc t -> VarSet.union acc (ty_vars t))
                       VarSet.empty lst

    | `TFunc (arg, ret) -> VarSet.union (ty_vars arg) (ty_vars ret)

    | `TSum ctors ->
        List.fold_left (fun acc (_, t) -> VarSet.union acc (ty_vars t))
                       VarSet.empty ctors

(*
 * Generalize a type. Returns a type scheme where every type variable
 * has been quantified
 *)
let gen (ty: [< ty]) =
    let vars = ty_vars ty in
    VarSet.fold (fun v acc -> `TSForall (v, acc)) vars (`TSTy ty)

(*
 * Instantiates a polymorphic type scheme by replacing all quantified
 * variables by fresh type variables
 *)
let rec inst sch = match sch with
    | `TSTy t -> t
    | `TSForall (v, sch) ->
        let var = next_var () in
        subst (`TVar var) v (inst sch)

(* The typing environment for polymorphic typing *)
type sch_env = (string * ty_sch) list

(*
 * Unification algorithm
 *)

module Unif = struct
    (*
     * equation system that stores all the constraints
     * encountered during type inference
     *)
    type equ = ty * ty

    (*
     * Be careful with the built-in equality especially over type terms and
     * type variables ; it will work for the moment, as it is structural,
     * but if we change (e.g. for optimisation purpose) something in the
     * representation, it may not work correctly anymore.
     *)

    (*
     * Check of a type variable occurence iside a type term
     *)
    let rec occur_check x (t: ty) = match t with
        | `TUnit | `TInt    -> false
        | `TVar v           -> (v = x)
        | `TTuple l         -> List.exists (occur_check x) l
        | `TFunc (arg, res) -> occur_check x arg || occur_check x res
        | `TSum l           -> List.exists (fun (_, ty) -> occur_check x ty) l

    (*
     * Exception raised whenever the unification is impossible
     * (maybe we should prefer option type, as it guarantees things through the
     * ocaml type system, while exception are not guarated to be caught)
     * TODO : have a more detailed failure system (implies to modify parsing...) !!
     *)
    exception ImpossibleToUnify

    (*
     * One of the main functions of unification.
     * Browse the given equation system and searches for an equation of the
     * form Var = Type. Other equations encountered are processed accordingly:
     *  - Equations between equivalent type constants are removed
     *  - Equations between incompatible types raise a type error
     *  - Equations between compatible structures are split and processed
     *    recursively
     * Finally, returns the first matching equation found, or None if none is
     * found.
     *)
    let rec get_var_eq se = match se with
        | [] -> None
        | eq :: rest ->
            (match eq with
                (* same varables ; remove the equation *)
                | (`TVar v1, `TVar v2) when v1 = v2 -> get_var_eq rest

                (* variable to be substituted *)
                | (`TVar v, t) | (t, `TVar v)       ->
                    if occur_check v t then
                        failwith "Recursive types are not allowed yet"
                    else Some ((v, t), rest)

                (*
                 * constants. must be put after the variables,
                 * otherwise, the _ would match it
                 *)
                | (`TUnit, `TUnit)                  -> get_var_eq rest
                | (`TUnit, _)                       -> raise ImpossibleToUnify
                | (`TInt, `TInt)                    -> get_var_eq rest
                | (`TInt, _)                        -> raise ImpossibleToUnify
                | (`TSum l1, `TSum l2)              ->
                    failwith "Unification of polymorphic \
                        variant types is not yet implemented"
                | (`TSum _, _)                      -> raise ImpossibleToUnify

                (*
                 * Splitting structures. Be careful with function when there
                 * will be subtypes.
                 *)
                | (`TTuple l1, `TTuple l2)          ->
                    (try get_var_eq (List.rev_append (List.combine l1 l2) rest)
                     with Invalid_argument "List.combine" ->
                         raise ImpossibleToUnify)
                | (`TTuple _, _)                    -> raise ImpossibleToUnify
                | (`TFunc (arg1, res1),
                   `TFunc (arg2, res2))             ->
                    get_var_eq ((arg1, arg2)::(res1, res2)::rest)
                | (`TFunc _, _)                     -> raise ImpossibleToUnify
            )

    (*
     * Unify function. Takes a system of equations and return an association table
     * that map each variable in the system to a type term.
     * Rely heavily on the above function (notice that both are tail-recursive).
     * (witch are NOT to be confused with polymorphic types)
     * Invariant: the variables that are bound in solved can not
     * appear in the remaining equations nor in type terms
     *)
    let unify (se: equ list) =
        let rec aux solved current = match get_var_eq current with
            | None -> solved
            | Some ((v, ty), sys) ->
                aux ((v, ty)::(List.map (fun (x, t) -> (x, subst ty v t)) solved))
                (List.map (fun (t1, t2) -> (subst ty v t1, subst ty v t2)) sys)
        in aux [] se
end

(*
 * Substitutions as first class values
 * (used by the immediate resolution inference algorithm)
 *)
module Subst: sig
    type t
    val empty: unit -> t
    val apply: t -> ty -> ty
    val merge: t -> (tvar * ty) list -> unit
    val unify: t -> Unif.equ list -> unit
    val to_alist: t -> (tvar * ty) list
end = struct
    type t = (tvar, ty) Hashtbl.t
    let empty () = Hashtbl.create 0

    (* apply the substitutions on a type *)
    let rec apply substs ty = match ty with
        | `TUnit | `TInt    -> ty
        | `TVar v           -> (try Hashtbl.find substs v with Not_found -> ty)
        | `TTuple l         -> `TTuple (List.map (apply substs) l)
        | `TFunc (arg, res) -> `TFunc (apply substs arg, apply substs res)
        | `TSum l           ->
            `TSum (List.map (fun (ctor, ty) -> (ctor, apply substs ty)) l)

    (*
     * Merges an associative list of (variable, type) returned by the
     * unification into our subsitutions map.
     * Assumes that type terms of alist do not contain the keys of alist
     * or of the subsitutions map (this also forbids recursive types)
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

    let to_alist substs = Hashtbl.fold (fun k v acc -> (k, v) :: acc) substs []
end
