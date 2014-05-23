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
}

let id     = ['a'-'z']['A'-'Z''a'-'z''_''0'-'9']*
let ctor   = ['A'-'Z']['A'-'Z''a'-'z''_''0'-'9']*
let intcst = ['0'-'9']+

rule token = parse
| [' ' '\t''\n']    { token lexbuf }
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
| '+'               { PLUS }
| '-'               { MINUS }
| '*'               { MULT }
| '/'               { DIV }
| eof               { EOF }
| _ as c            { failwith (Printf.sprintf "unknown token: %c" c) }
