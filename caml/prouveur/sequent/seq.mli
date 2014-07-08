open Def


(* choix_formule *)
(*
val fold_g : ('a -> sous_formule -> 'a) -> 'a -> 'a
val fold_d : ('a -> sous_formule -> 'a) -> 'a -> 'a
val find_g : (sous_formule -> bool) -> couple
val find_d : (sous_formule -> bool) -> couple
*)

val rm_ax : unit -> unit

val of_sous_formule : int -> unit
val of_sous_formule_CL : int -> unit

val n : unit -> int

val inf_n : int -> bool
val eq_n : int -> bool





val add_g : couple -> unit
val add_d : couple -> unit
val rm_g : couple -> unit
val rm_d : couple -> unit

val incr_n : unit -> unit
val decr_n : unit -> unit

val check_fauxL: unit -> bool
val check_id : unit -> sous_formule option


val nombre_imp_g : unit -> int
val nombre_imp_d : unit -> int
val nth_imp_g : int -> couple
val nth_imp_d : int -> couple
val reord_imp_g : int -> int -> unit
val reord_imp_d : int -> int -> unit

val var_g : unit -> string list



val choix_formule : unit -> quoi_faire * couple


val print : unit -> unit
