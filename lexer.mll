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
     * being built. The lexmap contains:
     *  - a copy of the whole program, filled
     *    by the read function when reading from
     *    an input_chan, or pre-filled when reading
     *    from a string
     *  - an offset to know where to read when we
     *    read from a string
     *  - the positions of the beginning of the lines,
     *    updated by the lexer.c
     *)
    type lexmap = {
        mutable contents: string ;
        mutable lines:    int array ;
        mutable offset:   int ;
    }

    let new_lexmap () = {
        contents = "" ; lines = [| 0 |] ; offset = 0
    }

    let lexmap_from_str str = {
        contents = str ; lines = [| 0 |] ; offset = 0
    }

    (*
     * Reads at most len bytes in str, starting at start,
     * and stores them at the beginning of buf. Returns
     * the number of characters read.
     *)
    let input_str str buf start len =
        let avail = String.length str - start in
        let len = if avail < len then avail else len in
        String.blit str start buf 0 len ;
        len

    (*
     * Used as input function by the lexer. Stores the
     * input in the code map for later use for error
     * reporting and sends it back to the lexer
     *)
    let read ic lexmap buf len =
        let count = input ic buf 0 len in
        let data = String.sub buf 0 count in
        lexmap.contents <- lexmap.contents ^ data ;
        count

    (*
     * In the case we read from a string, we don't need to
     * store data, but to update an offset that tells us
     * where to read on the next call
     *)
    let read_str lexmap buf len =
        let count = input_str (lexmap.contents) buf lexmap.offset len in
        lexmap.offset <- lexmap.offset + count ;
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
