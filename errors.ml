open Codemap

let span_err sess sp msg =
    let (s, e) = sp in
    let file = sess.cm.(s.fileno) in
        Printf.printf "%s:%d:%d: %d:%d \x1b[1;31merror: \x1b[1;37m%s\x1b[0m\n"
                      file.name s.line s.col e.line e.col msg ;
        if s.line == e.line then
            (*
             * The errorneous expression spans over a single line
             * Draw an arrow to point the exact position
             *)
            let line = Codemap.pos_line sess s in
            let pre  = Printf.sprintf "%s:%d" file.name s.line in
            let pad  = String.make (String.length pre + s.col) ' ' in
            let arr  = String.make (e.col - s.col - 1) '~' in
            Printf.printf "%s %s\n" pre line ;
            Printf.printf "%s\x1b[1;31m^%s\x1b[0m\n" pad arr
        else for i = s.line to e.line do
            let line = Codemap.line sess s.fileno i in
            Printf.printf "%s:%d %s\n" file.name i line
        done

let span_fatal sess sp msg =
    span_err sess sp msg ;
    failwith "Aborting due to previous errors"
