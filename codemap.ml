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
