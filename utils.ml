(* Utility functions *)

(* TODO: use Batteries *)
let hashtbl_map_inplace f map =
    let keys = Hashtbl.fold (fun k _ res -> k :: res) map [] in
    List.iter (fun key -> Hashtbl.replace map key (f (Hashtbl.find map key))) keys

let string_of_list lst ?sep:(s=" ; ") tostr = match lst with
    | []           -> ""
    | (car :: cdr) -> (List.fold_left
                          (fun acc e -> (Printf.sprintf "%s%s%s" acc s (tostr e)))
                          (tostr car) cdr)

