(* common type definitions for the AST *)

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
] and pattern = s_pattern Codemap.spanned

type s_basic_pattern = [
    | `PatUnit
    | `PatWildcard
    | `PatCst     of int
    | `PatVar     of string
    | `PatTup     of basic_pattern list
] and basic_pattern = s_basic_pattern Codemap.spanned

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
] and ast  = s_ast Codemap.spanned

type s_basic_ast = [
    | `Unit
    | `Cst     of int
    | `Var     of string
    | `Tuple   of basic_ast list
    | `If      of basic_ast * basic_ast * basic_ast
    | `Fun     of basic_pattern * basic_ast
    | `Let     of basic_pattern * basic_ast * basic_ast
    | `Match   of basic_ast * (basic_pattern * basic_ast) list
    | `Apply   of basic_ast * basic_ast
    | `BinOp   of bin_op * basic_ast * basic_ast
] and basic_ast  = s_basic_ast Codemap.spanned


(*
 * Those functions try to convert the AST into a more restricted type
 * and report an error if they find a construction that is not
 * allowed in the corresponding language level
 *)

let rec simple_pat (sp, pat) = (sp, match pat with
    | (`PatUnit     as a)
    | (`PatWildcard as a)
    | (`PatCst _    as a)
    | (`PatVar _    as a) -> a
    |  `PatTup lst        -> `PatTup (List.map simple_pat lst)
    | _                   -> failwith "invalid construction"
)

let rec simple_ast (sp, ast) = (sp, match ast with
    | (`Unit  as a)
    | (`Cst _ as a)
    | (`Var _ as a)            -> a
    |  `Tuple lst              -> `Tuple (List.map simple_ast lst)
    |  `If (ec, et, ef)        -> `If (simple_ast ec, simple_ast et, simple_ast ef)
    |  `Fun (pat, expr)        -> `Fun (simple_pat pat, simple_ast expr)
    |  `Let (pat, expr, body)  -> `Let (simple_pat pat, simple_ast expr, simple_ast body)

    |  `Match (expr, arms)     ->
         `Match (simple_ast expr,
                 List.map (fun (p, e) -> (simple_pat p, simple_ast e)) arms)

    |  `Apply (fn, arg)        -> `Apply (simple_ast fn, simple_ast arg)
    |  _                       -> failwith "invalid construction"
)

(* not-so-pretty-printing functions, for debug purposes *)

let binop_to_string op = match op with
    | `Plus  -> "+"
    | `Minus -> "-"
    | `Mult  -> "*"
    | `Div   -> "/"

let sprintf = Printf.sprintf

(* Returns a fully-parenthesized string representation of a pattern *)
let rec pat_to_string pat = match (snd pat) with
    | `PatUnit            -> "(unit)"
    | `PatWildcard        -> "_"
    | `PatCst i           -> sprintf "(%s)" (string_of_int i)
    | `PatVar v           -> sprintf "(%s)" v
    | `PatCtor (str, arg) -> sprintf "(%s %s)" str (pat_to_string arg)
    | `PatTup tup         -> sprintf "(%s)" (Utils.string_of_list tup ~sep:", "
                                                                  pat_to_string)

(* Returns a fully-parenthesized string representation of the AST *)
let rec ast_to_string ast = match (snd ast) with
    | `Unit               -> "(unit)"
    | `Cst i              -> sprintf "(%s)" (string_of_int i)
    | `Var v              -> sprintf "(%s)" v
    | `Ctor (str, ast)    -> sprintf "(%s %s)" str (ast_to_string ast)
    | `Tuple tup          -> sprintf "(%s)" (Utils.string_of_list tup ~sep:", "
                                                                  ast_to_string)

    | `If (e1, e2, e3)    ->
        sprintf "(if %s then %s else %s)"
                (ast_to_string e1) (ast_to_string e2) (ast_to_string e3)

    | `Fun (pat, expr)    ->
        sprintf "(fun %s -> %s)" (pat_to_string pat) (ast_to_string expr)

    | `Let (pat, e1, e2)  ->
        sprintf "(let %s = %s in %s)"
                (pat_to_string pat) (ast_to_string e1) (ast_to_string e2)

    | `Match (expr, arms) ->
        sprintf "(match %s with %s)" (ast_to_string expr)
                (Utils.string_of_list
                    arms ~sep:" | " (fun (pat, ast) ->
                                         (sprintf "%s -> %s"
                                             (pat_to_string pat)
                                             (ast_to_string ast))))

    | `BinOp (op, e1, e2) ->
        sprintf "(%s %s %s)"
                (ast_to_string e1) (binop_to_string op) (ast_to_string e2)

    | `Apply (func, arg)  ->
        sprintf "(%s %s)" (ast_to_string func) (ast_to_string arg)
