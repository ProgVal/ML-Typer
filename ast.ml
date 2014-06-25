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

let rec expr_of_pat (sp, pat) = (sp, match pat with
    | `PatUnit        -> `Unit
    | `PatWildcard    -> failwith "fixme: this shouldn't exist"
    | `PatCst c       -> `Cst c
    | `PatVar v       -> `Var v
    | `PatCtor (v, e) -> `Ctor (v, expr_of_pat e)
    | `PatTup t       -> `Tuple (List.map expr_of_pat t))

(* a possibly errorenous AST *)
(* TODO: way too much ASTs *)

type s_err_pattern = [
    | `PatUnit
    | `PatWildcard
    | `PatCst     of int
    | `PatVar     of string
    | `PatCtor    of string * err_pattern
    | `PatTup     of err_pattern list
    | `ParseError of string
] and err_pattern = s_err_pattern Codemap.spanned

type s_err_ast = [
    | `Unit
    | `Cst        of int
    | `Var        of string
    | `Ctor       of string * err_ast
    | `Tuple      of err_ast list
    | `If         of err_ast * err_ast * err_ast
    | `Fun        of err_pattern * err_ast
    | `Let        of err_pattern * err_ast * err_ast
    | `Match      of err_ast * (err_pattern * err_ast) list
    | `Apply      of err_ast * err_ast
    | `BinOp      of bin_op * err_ast * err_ast
    | `ParseError of string
] and err_ast  = s_err_ast Codemap.spanned

type defs = [
    | `Defs of (err_pattern * err_ast) list
    | `Expr of err_ast
    | `ParseError of string
] Codemap.spanned

(*
 * run the parser and checks the returned AST from parse errors
 * returns an AST that is guaranteed by typing not to contain
 * any errorneous construction, or None, if parse errors
 * has been found.
 * Calls the given callback on each error encountered, which
 * may be used to just report the error, or do something else,
 * accordingly.
 * TODO: if only we had monads... implement in Utils?
 *)
  

let rec check_pat err ((sp, pat): err_pattern) : pattern Utils.Maybe.t =
  let open Utils.Maybe in
  match pat with
    | (`PatUnit     as a)
    | (`PatWildcard as a)
    | (`PatCst _    as a)
    | (`PatVar _    as a) -> return (sp, a)

    | `PatCtor (v, pat)   ->
        (check_pat err pat) >>= (fun p -> return (sp, `PatCtor (v, p)))

    | `PatTup pats        ->
        (map_m (List.map (check_pat err) pats)) >>=
            (fun lst -> return (sp, `PatTup lst))

    | `ParseError e       -> err sp e ; None

let rec check err ((sp, ast): err_ast) : ast option =
    let open Utils.Maybe in match ast with
        | (`Unit  as a)
        | (`Cst _ as a)
        | (`Var _ as a)          -> return (sp, a)

        | `Ctor (v, arg)         ->
            (check err arg) >>= (fun a -> return (sp, `Ctor (v, a)))

        | `Tuple asts            ->
            (map_m (List.map (check err) asts)) >>=
                (fun lst -> return (sp, `Tuple lst))

        | `If (ec, et, ef)       ->
            bind3 (check err ec) (check err et) (check err ef)
                  (fun ec et ef -> return (sp, `If (ec, et, ef)))

        | `Fun (pat, expr)       ->
            bind2 (check_pat err pat) (check err expr)
                  (fun p e -> return (sp, `Fun (p, e)))

        | `Let (pat, expr, body) ->
            bind3 (check_pat err pat) (check err expr) (check err body)
                  (fun p e b -> Some (sp, `Let (p, e, b)))

        | `Match (expr, arms)    ->
            bind2 (check err expr)
                  (map_m (List.map (fun (p, a) ->
                                        bind2 (check_pat err p) (check err a)
                                              (fun p a -> return (p, a)))
                         arms))
                  (fun expr arms -> return (sp, `Match (expr, arms)))

        | `Apply (fn, arg)       ->
            bind2 (check err fn) (check err arg)
                  (fun f a -> return (sp, `Apply (f, a)))

        | `BinOp (op, opl, opr)  ->
            bind2 (check err opl) (check err opr)
                  (fun l r -> return (sp, `BinOp (op, l, r)))

        | `ParseError e          -> err sp e ; None

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

let rec simple_ast ((sp, ast): ast) = (sp, match ast with
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
    | `BinOp (op, opl, opr)    -> `BinOp (op, simple_ast opl, simple_ast opr)
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
