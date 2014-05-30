open Codemap

(*
 * Builds the AST for the current parse session
 *)
let parse sess =
    let contents = sess.cm.(0).contents in
    let lexbuf = Lexing.from_string contents in
    Parser.main Lexer.token lexbuf

let () =
    let sess = sess_open None in
    let result = parse sess in
    let (ty, equs) = Typer.infer sess [] result in
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
    print_newline () ;
    ( try
        let unif = Typer.unify equs in
        print_string "unified: " ;
        print_newline () ;
        List.iter (fun (v, t) ->
                   print_string (Typer.ty_to_string (Typer.TVar v)) ;
                   print_string " = " ;
                   print_string (Typer.ty_to_string t) ;
                   print_newline ())
                  unif
      with Typer.ImpossibleToUnify -> (
        print_string "typer error : unification is impossilbe" ;
        print_newline () ;
      )
    ) ;
    flush stdout
