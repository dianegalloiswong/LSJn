open Def


(*
open Preuve
open Contre_modele
open Rep
remplacés par :
*)
type regle = R_fauxL | R_Id | R_etL | R_etR | R_ouL | R_ouR | R_impL | R_impR
let vrai rep = rep
let preuve _ _ _ = true
let cmod _ _ = false
exception Continuer of bool
(***)

let rec prouvable () =
  Time.verif_timeout ();
  if Seq.check_fauxL () then preuve R_fauxL 0 [] else
  match Seq.check_id () with Some a -> preuve R_Id a [] | None ->
  let qf,c = Seq.choix_formule () in
  match qf with
    | QF_fauxL -> (*Preuve(P (R_fauxL,snd c,[]))*) assert false
    | QF_etL -> etL c
    | QF_ouR -> ouR c
    | QF_ouL -> ouL c
    | QF_etR -> etR c
    | QF_imp -> imp ()
    | QF_aucun -> 
(*Format.printf "Irr: "; Seq.print (); Format.printf "@.";*)
cmod (Seq.var_g()) []

and inversible1prem r_prem r_rev regle c =
  r_prem c;
  let rep = prouvable () in
  r_rev c;
  if vrai rep then preuve regle (snd c) [rep] else rep
and etL c = inversible1prem 
  Applique_regle.etL_prem 
  Applique_regle.etL_rev 
  R_etL c
and ouR c = inversible1prem 
  Applique_regle.ouR_prem 
  Applique_regle.ouR_rev 
  R_ouR c

and inversible2prem r1_prem r1_rev r2_prem r2_rev regle c =
  r1_prem c;
  let rep1 = prouvable () in
  r1_rev c;
  if vrai rep1 then
    begin
    r2_prem c;
    let rep2 = prouvable () in
    r2_rev c;
    if vrai rep2 then
      (*Preuve(P (regle,snd c,preuves[rep1;rep2]))*)
      preuve regle (snd c) [rep1;rep2]
    else rep2
    end
  else rep1
and ouL c = inversible2prem 
  Applique_regle.ouL1_prem 
  Applique_regle.ouL1_rev 
  Applique_regle.ouL2_prem 
  Applique_regle.ouL2_rev 
  R_ouL c
and etR c = inversible2prem 
  Applique_regle.etR1_prem 
  Applique_regle.etR1_rev 
  Applique_regle.etR2_prem 
  Applique_regle.etR2_rev 
  R_etR c
  
and impL c =
  Applique_regle.impL1_prem c;
  let rep1 = prouvable () in
  Applique_regle.impL1_rev c;
  if vrai rep1 then 
    begin
    Applique_regle.impL2_prem c;
    let rep2 = prouvable () in
    Applique_regle.impL2_rev c;
    if vrai rep2 then 
      begin
      Applique_regle.impL3_prem c;
      let rep3 = prouvable () in
      Applique_regle.impL3_rev c;
      if vrai rep3 then 
	preuve R_impL (snd c) [rep1;rep2;rep3]
      else
	raise (Continuer rep3)
      end
    else rep2
    end
  else rep1
and impR c =
  Applique_regle.impR1_prem c;
  let rep1 = prouvable () in
  Applique_regle.impR1_rev c;
  if vrai rep1 then 
    begin
    Applique_regle.impR2_prem c;
    let rep2 = prouvable () in
    Applique_regle.impR2_rev c;
    if vrai rep2 then 
      preuve R_impR (snd c) [rep1;rep2]
    else
      raise (Continuer rep2)
    end
  else rep1

and imp () =
  let nb_g = Seq.nombre_imp_g ()
  and nb_d = Seq.nombre_imp_d () in
  let rec aux_g k acc =
    if k = nb_g then 
      aux_d 0 acc
    else
      let c = Seq.nth_imp_g k in
      try 
	let rep = impL c in
	Seq.reord_imp_g (k+1) nb_g;
	rep
      with Continuer rep -> aux_g (k+1) (rep::acc)
  and aux_d k acc =
    if k = nb_d then 
      cmod (Seq.var_g()) acc
    else
      let c = Seq.nth_imp_d k in 
      try 
	let rep = impR c in
	Seq.reord_imp_d (k+1) nb_d;
	rep
      with Continuer rep -> aux_d (k+1) (rep::acc)
  in
  aux_g 0 []
  






let main formule =
  Init_sf_classe.main formule;
  Init_priorite.main ();

  let m = (Array.length !Global_ref.sf) - 1 in
  Seq.of_sous_formule m;

  prouvable ()


let main_CL formule =
  Init_sf_classe.main formule;
  Init_priorite.main ();

  let m = (Array.length !Global_ref.sf) - 1 in
  Seq.of_sous_formule_CL m;

  prouvable ()



