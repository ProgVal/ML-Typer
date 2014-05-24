(*
 * common type definitionss for the AST
 *)

(* a binary arithmetic operator *)
type bin_op =
  | Plus
  | Minus
  | Mult
  | Div

(*
 * pattern of a match arm, or of the left-hand
 * side of a let ... in form, or of a function
 * argument
 *)
type pattern =
  | PatUnit
  | PatWildcard
  | PatCst  of int
  | PatVar  of string
  | PatCtor of string * pattern
  | PatTup  of pattern list

(* internal representation of the AST *)
type ast =
  | Unit
  | Cst     of int
  | Var     of string
  | Ctor    of string * ast
  | Tuple   of ast list
  | If      of ast * ast * ast
  | Fun     of pattern * ast
  | Let     of pattern * ast * ast
  | Match   of ast * (pattern * ast) list
  | Apply   of ast * ast
  | BinOp   of bin_op * ast * ast

(* not-so-pretty-printing functions, for debug purposes *)

let binop_to_string op = match op with
  | Plus  -> "+"
  | Minus -> "-"
  | Mult  -> "*"
  | Div   -> "/"

let rec pat_to_string pat = match pat with
  | PatUnit             -> "(unit)"
  | PatWildcard         -> "_"
  | PatCst i            -> Printf.sprintf "(%s)" (string_of_int i)
  | PatVar v            -> Printf.sprintf "(%s)" v
  | PatCtor (str, arg)  -> Printf.sprintf "(%s %s)" str (pat_to_string arg)
  | PatTup []           -> failwith "empty tuple"
  | PatTup (car :: cdr) -> Printf.sprintf
                               "(%s)"
                               (List.fold_left
                                   (fun res pat -> 
                                       (Printf.sprintf "%s, %s" res 
                                                       (pat_to_string pat)))
                                   (pat_to_string car)
                                   cdr)

let rec ast_to_string ast = match ast with
  | Unit               -> "(unit)"
  | Cst i              -> Printf.sprintf "(%s)" (string_of_int i)
  | Var v              -> Printf.sprintf "(%s)" v
  | Ctor (str, ast)    -> Printf.sprintf "(%s %s)" str (ast_to_string ast)
  | Tuple []           -> failwith "empty tuple"
  | Tuple (car :: cdr) -> Printf.sprintf
                              "(%s)"
                              (List.fold_left
                                  (fun res ast ->
                                      (Printf.sprintf "%s, %s" res
                                                      (ast_to_string ast)))
                                  (ast_to_string car)
                                  cdr)
  | If (e1, e2, e3)    -> Printf.sprintf "(if %s then %s else %s)"
                                        (ast_to_string e1)
                                        (ast_to_string e2)
                                        (ast_to_string e3)
  | Fun (pat, expr)    -> Printf.sprintf "(fun %s -> %s)"
                                        (pat_to_string pat)
                                        (ast_to_string expr)
  | Let (pat, e1, e2)  -> Printf.sprintf "(let %s = %s in %s)" 
                                        (pat_to_string pat)
                                        (ast_to_string e1)
                                        (ast_to_string e2)
  | Match (expr, arms) -> Printf.sprintf
                              "(match %s with %s)"
                              (ast_to_string expr)
                              (List.fold_left
                                  (fun res (pat, ast) ->
                                      (Printf.sprintf "%s | %s -> %s" res
                                                      (pat_to_string pat)
                                                      (ast_to_string ast)))
                                  "" arms)
  | BinOp (op, e1, e2) -> Printf.sprintf "(%s %s %s)"
                                         (ast_to_string e1)
                                         (binop_to_string op)
                                         (ast_to_string e2)
  | Apply (func, arg)  -> Printf.sprintf "(%s %s)"
                                          (ast_to_string func)
                                          (ast_to_string arg)
