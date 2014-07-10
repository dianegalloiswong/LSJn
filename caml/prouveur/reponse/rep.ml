(*type t = Preuve of Preuve.t | CMod of Contre_modele.t

let vrai = function Preuve _ -> true | CMod _ -> false

let to_preuve = function Preuve pr -> pr | CMod _ -> assert false
let to_preuves = List.map to_preuve
let to_cmod = function Preuve _ -> assert false | CMod m -> m
let to_cmods = List.map to_cmod

let preuve regle sf replist = Preuve(Preuve.P (regle,sf,to_preuves replist))
let cmod sset replist = CMod(Contre_modele.M (sset,to_cmods replist))

exception Continuer of t
(* quand on trouve une réfutation d'un prémice non inversible (impL et impR) *)

let print_rep rep =
  if vrai rep then
    (Format.printf "vrai@.";
     if (*!details ||*) !Options.preuves then Preuve.print (to_preuve rep))
  else
    (Format.printf "faux@.";
     if (*!details ||*) !Options.cmods then Contre_modele.print (to_cmod rep));
  Format.printf "@."
*)

(***)

type t = bool
let est_vrai rep = rep
let vrai _ _ _ = true
let irr () = false
let cmod _ = false
let print rep =
  if rep then
    Format.printf "vrai@."
  else
    Format.printf "faux@."

let of_bool b = b (* pour prouveur_test.ml *)
(***)







(*




type t = 
  | Bool of bool
  | Preuve of Preuve.t 
  | Refut of Refutation.t
  | C_M of Contre_modele.t

let est_vrai = function 
  | Bool b -> b
  | Preuve _ -> true 
  | Refut _ -> false
  | C_M _ -> false


let to_preuve = function Preuve pr -> pr | _ -> assert false
let to_preuves = List.map to_preuve
let to_cmod = function C_M m -> m | _ -> assert false
let to_cmods = List.map to_cmod
let to_refut = function Refut r -> r | _ -> assert false
let to_refuts = List.map to_refut

type objectif = B | P | R | CM
let objectif = ref B





let vrai regle sf replist = match !objectif with
  | P -> Preuve(Preuve.P (regle,sf,to_preuves replist))
  | _ -> Bool true

let cmod replist = match !objectif with
  
  | CM -> C_M(Contre_modele.M (Seq.var_g(),to_cmods replist))
  | _ -> Bool false


let irr () = match !objectif with
  | R -> Refut(Refutation.R (Refutation.R_Irr,0,[]))
  | CM -> C_M(Contre_modele.M (Seq.var_g(),[]))
  | _ -> Bool false

let faux regle sf rep = match !objectif with
  | R -> Refut(Refutation.R (regle,sf,[to_refut rep]))
  | _ -> rep

let of_bool b = Bool b

let print = function
  | Bool b -> Format.printf "%s@." (if b then "vrai" else "faux")
  | Preuve pr -> Format.printf "vrai@."; Preuve.print pr
  | C_M m -> Format.printf "faux@."; Contre_modele.print m
  | Refut _ -> assert false
let print_rep = print


*)



