(*
 * builds an AST from a string
 * for debugging purposes in the OCaml toplevel
 *)
let parse str =
    let lexbuf = Lexing.from_string str in
    Parser.main Lexer.token lexbuf

(*
 * TODO: run typer
 * for now, just run the parser and
 * display the AST
 *)
let () =
    let lexbuf = Lexing.from_channel stdin in
    let result = Parser.main Lexer.token lexbuf in
    print_string (Ast.ast_to_string result) ;
    print_newline () ;
    flush stdout
