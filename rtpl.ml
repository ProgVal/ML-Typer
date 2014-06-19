(* Read-Type-Print-Loop *)

open Session

(* TODO: improve genericity by geleralizing the use of type schemes *)

let () =
    let tp = match Sys.argv.(1) with
        | "simple" ->
            (fun sess ast ->
                (Types.ty_to_string (Typers_simple.infer sess [] ast)))

        | "core"   ->
            (fun sess ast ->
                (Types.ty_to_string (Typers_core.infer sess [] ast)))

        | "poly"   ->
            (fun sess ast ->
                (Types.sty_to_string (Typers_poly.infer sess [] ast)))

        | _        -> failwith "unknown typing algorithm"
    in
    let rec iter () =
        Printf.printf "λ > " ;
        flush stdout ;
        let (sess, ast) = sess_open () in
        let ast = Ast.simple_ast ast in
        (try Printf.printf "type: %s\n" (tp sess ast)
         with Errors.Compile_failure -> ()) ;
        iter () in
    Printf.printf "\t\tRead Type Print Loop version 0.1. %s typer.\n\n"
                  Sys.argv.(1) ;
    iter ()
