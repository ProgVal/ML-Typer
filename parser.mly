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

%{ open Ast %}

%start main
%token EOF
%token OP CL
%token EQ COMMA PIPE ARR USCO
%token IF THEN ELSE FUN LET IN MATCH WITH
%token PLUS MINUS MULT DIV
%token <int> INT
%token <string> ID
%token <string> CTOR
%type  <Ast.ast> main

%%

main:
    | expr EOF                      { $1 }
    ;

expr:
    | arith                         { $1 }
    | expr arith                    { Apply ($1, $2) }
    ;

arith:
    | arith PLUS term               { BinOp (Plus, $1, $3) }
    | arith MINUS term              { BinOp (Minus, $1, $3) }
    | term                          { $1 }
    ;

term:
    | term MULT factor              { BinOp (Mult, $1, $3) }
    | term DIV factor               { BinOp (Div, $1, $3) }
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
    | IF expr THEN expr ELSE expr   { If ($2, $4, $6) }
    ;

fun_expr:
    | FUN pattern ARR expr          { Fun ($2, $4) }
    ;

let_expr:
    | LET pattern EQ expr IN expr   { Let ($2, $4, $6) }
    ;

match_expr:
    | MATCH expr WITH arm_list      { Match ($2, $4) }
    | MATCH expr WITH PIPE arm_list { Match ($2, $5) }
    ;

arm_list:
    | arm                           { [$1] }
    | arm PIPE arm_list             { $1 :: $3 }
    ;

arm:
    | pattern ARR expr              { ($1, $3) }
    ;

pattern:
    | USCO                          { PatWildcard }
    | ID                            { PatVar $1 }
    | OP tuple_pat CL               { PatTup $2 }
    | INT                           { PatCst $1 }
    | CTOR pattern                  { PatCtor ($1, $2) }
    | OP CL                         { PatUnit }
    | OP pattern CL                 { $2 }
    ;

tuple_pat:
    | pattern COMMA pattern         { [$1; $3] }
    | pattern COMMA tuple_pat       { $1 :: $3 }
    ;

entity:
    | ID                            { Var $1 }
    | INT                           { Cst $1 }
    | CTOR expr                     { Ctor ($1, $2) }
    | OP expr CL                    { $2 }
    | OP tuple CL                   { Tuple $2 }
    | OP CL                         { Unit }
    ;

tuple:
    | expr COMMA expr               { [$1; $3] }
    | expr COMMA tuple              { $1 :: $3 }
    ;
