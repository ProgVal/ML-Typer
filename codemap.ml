
(*
 * In memory "map" of a source file
 * Remembers the name of the file and its contents,
 * as well as the position of the beginning of each
 * line, to allow printing errorneous lines
 *)
type filemap = {
    name:     string    ;
    contents: string    ;
    lines:    int array ;
}

(*
 * The map of the whole program
 * basically maps of each source files
 *)
type codemap = filemap array

(*
 * The current compiling session.
 * For now, just contains the codemap, but additional
 * context informations may be added later
 *)
type session = {
    cm: codemap ;
}

(*
 * A position in the code. Defined by:
 *  - The index of the file in the codemap array
 *  - Line number, beginning at 1
 *  - Column number, beginning at 1
 *)
type pos = {
    fileno: int ;
    line:   int ;
    col:    int ;
}

(*
 * A span is a region of the code spanned by a
 * token or an non-terminal piece of code
 * Contains the beginning and the end of the region
 *)
type span = (pos * pos)

(*
 * Ast remembers the span associated to each node
 * This type decorates an arbitrary type with a span
 *)
type 's spanned = (span * 's)

(*
 * Opens a new compiler session from the given filename
 * If no filename is provided, uses stdin
 *)
let sess_open filename =
  let (ic, name) = match filename with
  | Some f -> (open_in f, f)
  | _ -> (stdin, "<stdin>") in
  let contents = ref "" in
  let lines = ref [||] in
  let pos = ref 0 in
  (try
      while true do
          let line = input_line ic in
          contents := !contents ^ line ^ "\n";
          lines := Array.append !lines [| !pos |] ;
          pos := !pos + (String.length line) +  1
      done
  with End_of_file -> ()) ;
  { cm = [| { name = name; contents = !contents; lines = !lines; } |]; }

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
