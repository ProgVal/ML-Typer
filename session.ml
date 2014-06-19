(*
 * The current compiling session.
 * For now, just contains the codemap, but additional
 * context informations may be added later
 *)

open Codemap

type session = {
    cm: codemap ;
}

(*
 * Opens a new compiler session from the given filename
 * If no filename is provided, uses stdin
 * Builds the AST for the current parse session and
 * returns a pair (session, ast)
 *)
let sess_open ?file () =
  let (ic, name) = match file with
  | Some f -> (open_in f, f)
  | _ -> (stdin, "<stdin>") in
  let lexmap = Lexer.new_lexmap () in
  let lexbuf = Lexing.from_function (Lexer.read ic lexmap) in
  let ast    = Parser.main (Lexer.token lexmap) lexbuf in
  let fmap   = {
      name = name;
      contents = lexmap.Lexer.contents;
      lines = lexmap.Lexer.lines;
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
        name = "<toplevel>";
        contents = contents;
        lines = lexmap.Lexer.lines;
    } in
    ({ cm = [| fmap |]; }, ast)

(*
 * Returns the position of the beginning of the line
 * that contains the given position
 * The position is returned as a byte offset, starting
 * at 0 (the beginning of the file)
 *)
let line_pos sess pos =
    let (file, i) = (pos.fileno, pos.line) in
    let file = sess.cm.(file) in
    file.lines.(i - 1)

(*
 * Returns the end of the line, i.e. the last character
 * before the beginning of the next line
 *)
let line_end sess pos =
    let (file, i) = (pos.fileno, pos.line) in
    let file = sess.cm.(file) in
    if i >= Array.length file.lines
    then String.length file.contents
    else file.lines.(i)

(*
 * Returns the line containg the given position
 *)
let pos_line sess pos =
    let spos = line_pos sess pos in
    let epos = line_end sess pos in
    let file = sess.cm.(pos.fileno) in
    String.sub file.contents spos (epos - spos - 1)

(*
 * Returns the ith line of the given file
 *)
let line sess f i =
    pos_line sess { fileno = f; line = i; col = 1 }

