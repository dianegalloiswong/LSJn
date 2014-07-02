type t = Preuve of Preuve.t | CMod of Contre_modele.t

let vrai = function Preuve _ -> true | CMod _ -> false

let preuve = function Preuve pr -> pr | CMod _ -> assert false
let preuves = List.map preuve
let cmod = function Preuve _ -> assert false | CMod m -> m
let cmods = List.map cmod


exception Continuer of t
(* quand on trouve une réfutation d'un prémice non inversible (impL et impR) *)

open Def

let print_rep rep =
  if vrai rep then
    (Format.printf "vrai@.";
     if (*!details ||*) !Options.preuves then Preuve.print (preuve rep))
  else
    (Format.printf "faux@.";
     if (*!details ||*) !Options.cmods then Contre_modele.print (cmod rep))

(*
let print_rep rep =
  if vrai rep then
    Format.printf "vrai@."
  else
    Format.printf "faux@."
*)
