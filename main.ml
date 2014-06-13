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

(*
 * Opens a new compiler session from a string containing the
 * program text. Builds the AST and returns a pair (session, ast)
 *)
let sess_open_str contents =
    let lexmap = Lexer.lexmap_from_str contents in
    let lexbuf = Lexing.from_function (Lexer.read_str lexmap) in
    let ast    = Parser.main (Lexer.token lexmap) lexbuf in
    let fmap   = {
        Codemap.name = "<toplevel>";
        Codemap.contents = contents;
        Codemap.lines = lexmap.Lexer.lines;
    } in
    ({ cm = [| fmap |]; }, ast)

let () =
    let (sess, result) = sess_open None in
    (* Uncomment to run in JS: *)
    (* let (sess, result) = sess_open_str "let x = fun y -> y in x 3" in *)
    print_string "FIRST ALGORITHM : " ;
    print_newline () ;
    let (ty, equs) = Typer.infer sess [] result in
    print_string "type: " ;
    print_string (Types.ty_to_string ty) ;
    print_newline () ;
    print_string "equs: " ;
    print_newline () ;
    List.iter (fun (x, y) -> print_string (Types.ty_to_string x) ;
                             print_string " = " ;
                             print_string (Types.ty_to_string y) ;
                             print_newline ())
              equs ;
    print_newline () ;
    ( try
        let unif = Types.Unif.unify equs in
        print_string "unified: " ;
        print_newline () ;
        List.iter (fun (v, t) ->
                   print_string (Types.ty_to_string (`TVar v)) ;
                   print_string " = " ;
                   print_string (Types.ty_to_string t) ;
                   print_newline ())
                  unif
      with Types.Unif.ImpossibleToUnify -> (
        print_string "typer error : unification is impossible" ;
        print_newline () ;
      )
    ) ;

    print_newline () ;
    print_newline () ;

    print_string "SECOND ALGORITHM : " ;
    print_newline () ;
    let (ty, binds) = Immtyper.infer sess [] result in
    print_string "type: " ;
    print_string (Types.ty_to_string ty) ;
    print_newline () ;
    print_string "bindings: " ;
    print_newline () ;
    (* TODO: print subst
    Hashtbl.iter (fun v t ->
                   print_string (Types.ty_to_string (`TVar v)) ;
                   print_string " = " ;
                   print_string (Types.ty_to_string t) ;
                   print_newline ()
                 )
              binds ;
    *)
    print_newline () ;
    flush stdout
