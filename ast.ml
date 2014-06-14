(*
 * common type definitionss for the AST
 *)
open Codemap

(* a binary arithmetic operator *)
type bin_op = [
  | `Plus
  | `Minus
  | `Mult
  | `Div
  ]

(*
 * pattern of a match arm, or of the left-hand
 * side of a let ... in form, or of a function
 * argument
 *)
type s_pattern = [
  | `PatUnit
  | `PatWildcard
  | `PatCst     of int
  | `PatVar     of string
  | `PatCtor    of string * pattern
  | `PatTup     of pattern list
  ] and pattern = s_pattern spanned

(* internal representation of the AST *)
type s_ast = [
  | `Unit
  | `Cst     of int
  | `Var     of string
  | `Ctor    of string * ast
  | `Tuple   of ast list
  | `If      of ast * ast * ast
  | `Fun     of pattern * ast
  | `Let     of pattern * ast * ast
  | `Match   of ast * (pattern * ast) list
  | `Apply   of ast * ast
  | `BinOp   of bin_op * ast * ast
  ] and ast  = s_ast spanned

(* not-so-pretty-printing functions, for debug purposes *)

let binop_to_string op = match op with
  | `Plus  -> "+"
  | `Minus -> "-"
  | `Mult  -> "*"
  | `Div   -> "/"

open Printf

let rec pat_to_string pat = match (snd pat) with
  | `PatUnit             -> "(unit)"
  | `PatWildcard         -> "_"
  | `PatCst i            -> sprintf "(%s)" (string_of_int i)
  | `PatVar v            -> sprintf "(%s)" v
  | `PatCtor (str, arg)  -> sprintf "(%s %s)" str (pat_to_string arg)
  | `PatTup tup          -> sprintf "(%s)" (Utils.string_of_list tup ~sep:", "
                                                                 pat_to_string)

let rec ast_to_string ast = match (snd ast) with
  | `Unit               -> "(unit)"
  | `Cst i              -> sprintf "(%s)" (string_of_int i)
  | `Var v              -> sprintf "(%s)" v
  | `Ctor (str, ast)    -> sprintf "(%s %s)" str (ast_to_string ast)
  | `Tuple tup          -> sprintf "(%s)" (Utils.string_of_list tup ~sep:", "
                                                                ast_to_string)
  | `If (e1, e2, e3)    -> sprintf "(if %s then %s else %s)" (ast_to_string e1)
                                                             (ast_to_string e2)
                                                             (ast_to_string e3)
  | `Fun (pat, expr)    -> sprintf "(fun %s -> %s)" (pat_to_string pat)
                                                    (ast_to_string expr)
  | `Let (pat, e1, e2)  -> sprintf "(let %s = %s in %s)" (pat_to_string pat)
                                                         (ast_to_string e1)
                                                         (ast_to_string e2)
  | `Match (expr, arms) -> sprintf "(match %s with %s)" (ast_to_string expr)
                                   (Utils.string_of_list
                                     arms ~sep:" | " (fun (pat, ast) ->
                                                       (sprintf "%s -> %s"
                                                       (pat_to_string pat)
                                                       (ast_to_string ast))))
  | `BinOp (op, e1, e2) -> sprintf "(%s %s %s)" (ast_to_string e1)
                                                (binop_to_string op)
                                                (ast_to_string e2)
  | `Apply (func, arg)  -> sprintf "(%s %s)" (ast_to_string func)
                                             (ast_to_string arg)
