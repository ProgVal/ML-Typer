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




(* === Monads === *)

module type SimpleMonad = sig
    type 'a t
    val bind : 'a t -> ('a -> 'b t) -> 'b t
    val return : 'a -> 'a t
end

module type Monad = sig
    include SimpleMonad
    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
    val ( >> ) : 'a t -> (unit -> 'b t) -> 'b t
    val join : 'a t t -> 'a t
    val map : ('a -> 'b) -> 'a t -> 'b t
    val bind2 : 'a t -> 'b t -> ('a -> 'b -> 'c t) -> 'c t
    val bind3 : 'a t -> 'b t -> 'c t -> ('a -> 'b -> 'c -> 'd t) -> 'd t
    val map_m : 'a t list -> 'a list t
end


module MakeMonad (M : SimpleMonad) = struct
    include M

    let ( >>= )    = bind
    let ( >> ) m f = bind m (fun _ -> f ())

    let join mm = bind mm (fun x -> x)

    let map f m = bind m (fun x -> return (f x))

    let bind2 m_a m_b f     = m_a >>= (fun a -> m_b >>= (f a))
    let bind3 m_a m_b m_c f = m_a >>= (fun a -> bind2 m_b m_c (f a))

    let map_m lstm =
        let f m1 m2 =
            let g a b = return (a :: b) in
            bind2 m1 m2 g
        in List.fold_right f lstm (return [])
end

(* standard implementations *)

module Maybe : Monad with type 'a t = 'a option =
    MakeMonad (struct
                   type 'a t = 'a option

                   let bind opt f = match opt with
                       | None -> None
                       | Some x -> f x

                    let return x = Some x
               end)
