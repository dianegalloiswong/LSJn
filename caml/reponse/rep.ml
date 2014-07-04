type t = Preuve of Preuve.t | CMod of Contre_modele.t

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


(***)
(* pour ne pas encombrer la mémoire avec les arbres de preuve/contre-modèle *)
type t' = bool
let vrai rep = rep
let preuve _ _ _ = true
let cmod _ _ = false
exception Continuer of t'
let print_rep rep =
  if vrai rep then
    Format.printf "vrai@."
  else
    Format.printf "faux@."
(***)








let print = print_rep
