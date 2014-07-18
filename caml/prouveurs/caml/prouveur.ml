open Def



open Preuve
(*open Contre_modele
*)
open Rep
(*remplacés par :

type regle = R_fauxL | R_Id | R_etL | R_etR | R_ouL | R_ouR | R_impL | R_impR
let vrai rep = rep
let preuve _ _ _ = true
let cmod _ _ = false
exception Continuer of bool*)
(***)


let main formule =
  Init_sf_classe.main formule;
  Init_priorite.main ();

  let m = (Array.length !Global_ref.sf) - 1 in
  Seq.of_sous_formule m;

  Prouvable.prouvable ()


let main_CL formule =
  Init_sf_classe.main formule;
  Init_priorite.main ();

  let m = (Array.length !Global_ref.sf) - 1 in
  Seq.of_sous_formule_CL m;

  Prouvable.prouvable ()



