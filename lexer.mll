(*
 * Lexer for a simple ML-like language
 * see parser.mly for a more detailed description
 * of the supported features
 *)

{
    open Parser

    let keywords = Hashtbl.create 0
    let () =
        List.iter (fun (kwd, tok) -> Hashtbl.add keywords kwd tok)
            [("ifz",   IF);
             ("then",  THEN);
             ("else",  ELSE);
             ("fun",   FUN);
             ("let",   LET);
             ("in",    IN);
             ("match", MATCH);
             ("with",  WITH)]

    (*
     * Temporary representation of a codemap
     * being built
     *)
    type lexmap = {
        mutable contents: string ;
        mutable lines:    int array ;
        mutable offset:   int ;
    }

    let new_lexmap () = {
        contents = "" ; lines = [|0|] ; offset = 0
    }

    (*
     * Used as input function by the lexer. Stores the
     * input in the code map for later use for error
     * reporting and sends it back to the lexer
     *)
    let read ic lexmap buf len =
        lexmap.offset <- String.length lexmap.contents ;
        let count = input ic buf 0 len in
        let data = String.sub buf 0 count in
        lexmap.contents <- lexmap.contents ^ data ;
        count
}

let id     = ['a'-'z']['A'-'Z''a'-'z''_''0'-'9']*
let ctor   = ['A'-'Z']['A'-'Z''a'-'z''_''0'-'9']*
let intcst = ['0'-'9']+

rule token lexmap = parse
| [' ' '\t']        { token lexmap lexbuf }
| '\n'              {
                        let pos = lexbuf.Lexing.lex_curr_p in
                        lexmap.lines <- Array.append lexmap.lines [|
                            pos.Lexing.pos_cnum |] ;
                        lexbuf.Lexing.lex_curr_p <- { pos with
                            Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
                            Lexing.pos_bol = pos.Lexing.pos_cnum;
                        } ;
                        token lexmap lexbuf
                    }
| id as str         { try Hashtbl.find keywords str
                      with Not_found -> ID str }
| ctor as str       { CTOR str }
| intcst as i       { INT (int_of_string i) }
| '('               { OP }
| ')'               { CL }
| '='               { EQ }
| ','               { COMMA }
| '|'               { PIPE }
| "->"              { ARR }
| '_'               { USCO }
| '+'               { PLUS }
| '-'               { MINUS }
| '*'               { MULT }
| '/'               { DIV }
| eof               { EOF }
| ";;"              { EOF }
| _ as c            { failwith (Printf.sprintf "unknown token: %c" c) }
