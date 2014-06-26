(* Read-Type-Print-Loop *)

open Session

(* TODO: improve genericity by geleralizing the use of type schemes *)

let () =
    if (Array.length Sys.argv) <> 2 then
        Printf.printf "Syntax: %s {simple,core,poly}\n" Sys.argv.(0)
    else (
        let open Utils.Maybe in
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
                let (sess, (sp, defs)) = sess_open () in
                match defs with
                    | `ParseError err ->
                        Errors.span_err sess sp err

                    | `Expr ast ->
                        ignore (
                            (Ast.check (Errors.span_err sess) ast) >>=
                                (fun ast ->
                                    let ast = Ast.simple_ast ast in
                                    let sty = Typer.infer sess [] ast in
                                    Printf.printf "type: %s\n"
                                                  (Types.sty_to_string sty) ;
                                    None ))

                    | `Defs [] -> exit 0

                    | `Defs defs ->
                        (List.iter
                            (fun (pat, expr) ->
                                ignore (
                                    bind2
                                        (Ast.check_pat (Errors.span_err sess) pat)
                                        (Ast.check (Errors.span_err sess) expr)
                                        (fun pat expr ->
                                            (*
                                             * For now, we just tranform defs into
                                             * lets to pass them to the typer
                                             * TODO: extract bindings to keep
                                             * environment
                                             *)
                                            let ast =
                                                (Ast.simple_ast
                                                    (sp, (`Let (pat, expr,
                                                                Ast.expr_of_pat pat))))
                                            in
                                            let sty = Typer.infer sess [] ast in
                                            Printf.printf "type: %s\n"
                                                          (Types.sty_to_string sty) ;
                                            None )))
                            defs)
             with Errors.CompileFailure -> ()) ;
            iter () in
        Printf.printf "\t\tRead Type Print Loop version 0.1. %s typer.\n\n"
                      Sys.argv.(1) ;
        iter ()
    )
