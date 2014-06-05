open Codemap

(*
 * Opens a new compiler session from the given filename
 * If no filename is provided, uses stdin
 * Builds the AST for the current parse session and
 * returns a pair (session, ast)
 *)
let sess_open filename =
  let (ic, name) = match filename with
  | Some f -> (open_in f, f)
  | _ -> (stdin, "<stdin>") in
  let lexmap = Lexer.new_lexmap () in
  let lexbuf = Lexing.from_function (Lexer.read ic lexmap) in
  let ast    = Parser.main (Lexer.token lexmap) lexbuf in
  let fmap   = {
      Codemap.name = name;
      Codemap.contents = lexmap.Lexer.contents;
      Codemap.lines = lexmap.Lexer.lines;
  } in
  ({ cm = [| fmap |]; }, ast)

let () =
    let (sess, result) = sess_open None in
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
        print_string "typer error : unification is impossible" ;
        print_newline () ;
      )
    ) ;
    flush stdout
