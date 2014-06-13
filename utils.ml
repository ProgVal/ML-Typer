(* Utility functions *)

(* TODO: use Batteries *)
let hashtbl_map_inplace f map =
    let keys = Hashtbl.fold (fun k _ res -> k :: res) map [] in
    List.iter (fun key -> Hashtbl.replace map key (f (Hashtbl.find map key))) keys

