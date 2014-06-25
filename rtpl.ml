(* Read-Type-Print-Loop *)

open Session

(* TODO: improve genericity by geleralizing the use of type schemes *)

let () =
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
            let (sess, defs) = sess_open () in
            match defs with
                | [] -> exit 0
                | defs ->
                    (List.iter
                        (fun (sp, d) -> match d with
                            | `ParseError err ->
                                Errors.span_err sess sp err ; ()

                            | `Def (pat, expr) ->
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
                                        Printf.printf
                                            "type: %s\n"
                                            (Types.sty_to_string sty) ;
                                        None ) ; ())
                        defs)
         with Errors.CompileFailure -> ()) ;
        iter () in
    Printf.printf "\t\tRead Type Print Loop version 0.1. %s typer.\n\n"
                  Sys.argv.(1) ;
    iter ()
