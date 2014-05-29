(*
 * builds an AST from a string
 * for debugging purposes in the OCaml toplevel
 *)
let parse str =
    let lexbuf = Lexing.from_string str in
    Parser.main Lexer.token lexbuf

let () =
    let lexbuf = Lexing.from_channel stdin in
    let result = Parser.main Lexer.token lexbuf in
    print_string (Ast.ast_to_string result) ;
    let (ty, equs) = Typer.infer [] result in
    print_newline () ;
    print_string "type: " ;
    print_string (Typer.ty_to_string ty) ;
    print_newline () ;
    print_string "equs: " ;
    print_newline () ;
    List.iter (fun (x, y) -> print_string (Typer.ty_to_string x) ;
                             print_string " = " ;
                             print_string (Typer.ty_to_string y) ;
                             print_newline ())
              equs ;
    let unif = Typer.unify equs in
    print_newline () ;
    print_string "unified: " ;
    print_newline () ;
    List.iter (fun (v, t) -> print_string (Typer.ty_to_string (Typer.TVar v)) ;
                             print_string " = " ;
                             print_string (Typer.ty_to_string t) ;
                             print_newline ())
              unif ;
    flush stdout
