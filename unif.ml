(*
 * Unification
 *)

open Type

(*
 * equation system that stores all the constraints
 * encountered during type inference
 *)
type equ = (ty * ty)
type equ_sys = equ list


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
