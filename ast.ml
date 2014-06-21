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

let rec check_pat ((sp, pat): err_pattern) err : pattern option = match pat with
    | (`PatUnit     as a)
    | (`PatWildcard as a)
    | (`PatCst _    as a)
    | (`PatVar _    as a) -> Some (sp, a)

    | `PatCtor (v, pat) ->
        (match check_pat pat err with
            | Some pat -> Some (sp, (`PatCtor (v, pat)))
            | None     -> None)

    | `PatTup pats ->
        (match
            (List.fold_left
                (fun acc pat -> match (check_pat pat err, acc) with
                    | (Some a, Some l) -> Some (a :: l)
                    | _                -> None)
                (Some []) pats)
        with
            | Some l -> Some (sp, `PatTup l)
            | None   -> None)

    | `ParseError e          -> err sp e ; None

let rec check ((sp, ast): err_ast) err : ast option = match ast with
    | (`Unit  as a)
    | (`Cst _ as a)
    | (`Var _ as a)          -> Some (sp, a)

    | `Ctor (v, arg)         ->
        (match check arg err with
            | Some ast -> Some (sp, (`Ctor (v, ast)))
            | None     -> None)

    | `Tuple asts            ->
        (match
            (List.fold_left
                (fun acc ast -> match (check ast err, acc) with
                     | (Some a, Some l) -> Some (a :: l)
                     | _                -> None)
                (Some []) asts)
        with
            | Some l -> Some (sp, `Tuple l)
            | None   -> None)

    | `If (ec, et, ef)       ->
        (match (check ec err, check et err, check ef err) with
            | (Some ec, Some et, Some ef) -> Some (sp, (`If (ec, et, ef)))
            | _                           -> None)

    | `Fun (pat, expr)       ->
        (match (check_pat pat err, check expr err) with
            | (Some pat, Some expr) -> Some (sp, (`Fun (pat, expr)))
            | _                     -> None)

    | `Let (pat, expr, body) ->
        (match (check_pat pat err, check expr err, check body err) with
            | (Some p, Some expr, Some body) -> Some (sp, (`Let (p, expr, body)))
            | _                              -> None)

    | `Match (expr, arms)    ->
        let expr = check expr err in
        let arms = List.fold_left
                       (fun acc (pat, ast) ->
                            match (check_pat pat err, check ast err, acc) with
                                | (Some p, Some a, Some l) -> Some ((p, a) :: l)
                                | _                        -> None)
                       (Some []) arms
        in (match (expr, arms) with
            | (Some expr, Some arms) -> Some (sp, (`Match (expr, arms)))
            | _                      -> None)

    | `Apply (fn, arg)       ->
        (match (check fn err, check arg err) with
            | (Some fn, Some arg) -> Some (sp, (`Apply (fn, arg)))
            | _                   -> None)

    | `BinOp (op, opl, opr)  ->
        (match (check opl err, check opr err) with
            | (Some opl, Some opr) -> Some (sp, (`BinOp (op, opl, opr)))
            | _                    -> None)

    | `ParseError e          -> err sp e ; None
;;

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
