(* Read-Type-Print-Loop *)

open Session

(* TODO: improve genericity by geleralizing the use of type schemes *)

let () =
    let typer = match Sys.argv.(1) with
      | "simple" -> (module Typers.Simple : Typers.BasicPolyTyper)
      | "core"   -> (module Typers.Core   : Typers.BasicPolyTyper)
      | "poly"   -> (module Typers.Poly   : Typers.BasicPolyTyper)
      | _        -> failwith "unknown typing algorithm"
    in
    let module Typer : Typers.BasicPolyTyper = (val typer) in
    let rec iter () =
        Printf.printf "λ > " ;
        flush stdout ;
        (try 
            let (sess, ast) = sess_open () in
            let ast = match ast with
                | Some ast ->
                    Ast.check
                        ast
                        (fun sp e -> Errors.span_err sess sp "syntax error")
                | None -> exit 0
            in
            match ast with
                | Some ast ->
                    let ast = Ast.simple_ast ast in
                    let sty = Typer.infer sess [] ast in
                    Printf.printf "type: %s\n" (Types.sty_to_string sty)

                | None     -> iter ()
         with Errors.CompileFailure -> ()) ;
        iter () in
    Printf.printf "\t\tRead Type Print Loop version 0.1. %s typer.\n\n"
                  Sys.argv.(1) ;
    iter ()
