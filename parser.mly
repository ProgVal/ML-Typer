/*
 * Parser for a simple ML-like language
 * supported features:
 *   - basic integer arithmetics
 *   - tuples
 *   - sum types
 *   - conditional expressions
 *   - unary functions
 *   - let ... in form
 *   - pattern matching
 */

%{
    open Ast
    open Codemap
    open Lexing

    let range s e =
        let sp = rhs_start_pos s in
        let ep = rhs_end_pos e in
        let sp = {
            fileno = 0 ;
            line = sp.pos_lnum ;
            col = sp.pos_cnum - sp.pos_bol + 1
        } in
        let ep = {
            fileno = 0 ;
            line = ep.pos_lnum ;
            col = ep.pos_cnum - ep.pos_bol + 1
        } in
        (sp, ep)

%}

%start main
%token EOF
%token OP CL
%token EQ COMMA PIPE ARR USCO
%token IF THEN ELSE FUN LET IN MATCH WITH
%token PLUS MINUS MULT DIV
%token <int> INT
%token <string> ID
%token <string> CTOR
%type  <Ast.err_ast> main

%%

main:
    | expr EOF                      { $1 }
    | error EOF                     { (range 1 1, `ParseError "") }
    ;

expr:
    | arith                         { $1 }
    | expr arith                    { (range 1 2, `Apply ($1, $2)) }
    ;

arith:
    | arith PLUS term               { (range 1 3, `BinOp (`Plus, $1, $3)) }
    | arith MINUS term              { (range 1 3, `BinOp (`Minus, $1, $3)) }
    | term                          { $1 }
    ;

term:
    | term MULT factor              { (range 1 3, `BinOp (`Mult, $1, $3)) }
    | term DIV factor               { (range 1 3, `BinOp (`Div, $1, $3)) }
    | factor                        { $1 }
    ;

factor:
    | if_expr                       { $1 }
    | fun_expr                      { $1 }
    | let_expr                      { $1 }
    | match_expr                    { $1 }
    | entity                        { $1 }
    ;

if_expr:
    | IF expr THEN expr ELSE expr   { (range 1 6, `If ($2, $4, $6)) }
    ;

fun_expr:
    | FUN pattern ARR expr          { (range 1 4, `Fun ($2, $4)) }
    ;

let_expr:
    | LET pattern EQ expr IN expr   { (range 1 6, `Let ($2, $4, $6)) }
    ;

match_expr:
    | MATCH expr WITH arm_list      { (range 1 4, `Match ($2, snd $4)) }
    | MATCH expr WITH PIPE arm_list { (range 1 5, `Match ($2, snd $5)) }
    ;

arm_list:
    | arm                           { (range 1 1, [snd $1]) }
    | arm PIPE arm_list             { (range 1 3, snd $1 :: snd $3) }
    ;

arm:
    | pattern ARR expr              { (range 1 3, ($1, $3)) }
    ;

pattern:
    | USCO                          { (range 1 1, `PatWildcard) }
    | ID                            { (range 1 1, `PatVar $1) }
    | OP tuple_pat CL               { (range 1 3, `PatTup (snd $2)) }
    | INT                           { (range 1 1, `PatCst $1) }
    | CTOR pattern                  { (range 1 2, `PatCtor ($1, $2)) }
    | OP CL                         { (range 1 2, `PatUnit) }
    | OP error CL                   { (range 2 2, `ParseError "") }
    | OP pattern CL                 { $2 }
    ;

tuple_pat:
    | pattern COMMA pattern         { (range 1 3, [$1; $3]) }
    | pattern COMMA tuple_pat       { (range 1 3, $1 :: snd $3) }
    ;

entity:
    | ID                            { (range 1 1, `Var $1) }
    | INT                           { (range 1 1, `Cst $1) }
    | CTOR expr                     { (range 1 2, `Ctor ($1, $2)) }
    | OP CL                         { (range 1 2, `Unit) }
    | OP tuple CL                   { (range 1 3, `Tuple (snd $2)) }
    | OP error CL                   { (range 2 2, `ParseError "") }
    | OP expr CL                    { $2 }
    ;

tuple:
    | expr COMMA expr               { (range 1 3, [$1; $3]) }
    | expr COMMA tuple              { (range 1 3, $1 :: snd $3) }
    ;
