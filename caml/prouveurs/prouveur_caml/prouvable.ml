
open Def

open Rep



let fauxL () = vrai Preuve.R_fauxL 0 []

let id a = vrai Preuve.R_Id a []

let irr () = 
  if !Options.irr then (Format.printf "Irr: "; Seq.print (); Format.printf "@.");
  Rep.irr ()





let rec prouvable () =
  Time.verif_timeout ();
  if Seq.check_fauxL () then fauxL () else
  match Seq.check_id () with Some a -> id a | None ->
  let qf,c = Seq.choix_formule () in
  match qf with
    | Seq.QF_etL -> etL c
    | Seq.QF_ouR -> ouR c
    | Seq.QF_ouL -> ouL c
    | Seq.QF_etR -> etR c
    | Seq.QF_imp -> imp ()
    | Seq.QF_aucun -> irr ()

and inversible1prem r_prem r_rev regle c =
  r_prem c;
  let rep = prouvable () in
  r_rev c;
  if est_vrai rep then vrai regle (snd c) [rep] else rep
and etL c = inversible1prem 
  Effet_regle_sur_sequent.etL_prem 
  Effet_regle_sur_sequent.etL_rev 
  Preuve.R_etL c
and ouR c = inversible1prem 
  Effet_regle_sur_sequent.ouR_prem 
  Effet_regle_sur_sequent.ouR_rev 
  Preuve.R_ouR c

and inversible2prem r1_prem r1_rev r2_prem r2_rev regle c =
  r1_prem c;
  let rep1 = prouvable () in
  r1_rev c;
  if est_vrai rep1 then
    begin
    r2_prem c;
    let rep2 = prouvable () in
    r2_rev c;
    if est_vrai rep2 then
      vrai regle (snd c) [rep1;rep2]
    else rep2
    end
  else rep1
and ouL c = inversible2prem 
  Effet_regle_sur_sequent.ouL1_prem 
  Effet_regle_sur_sequent.ouL1_rev 
  Effet_regle_sur_sequent.ouL2_prem 
  Effet_regle_sur_sequent.ouL2_rev 
  Preuve.R_ouL c
and etR c = inversible2prem 
  Effet_regle_sur_sequent.etR1_prem 
  Effet_regle_sur_sequent.etR1_rev 
  Effet_regle_sur_sequent.etR2_prem 
  Effet_regle_sur_sequent.etR2_rev 
  Preuve.R_etR c
  
and impL c =
  Effet_regle_sur_sequent.impL1_prem c;
  let rep1 = prouvable () in
  Effet_regle_sur_sequent.impL1_rev c;
  if est_vrai rep1 then 
    begin
    Effet_regle_sur_sequent.impL2_prem c;
    let rep2 = prouvable () in
    Effet_regle_sur_sequent.impL2_rev c;
    if est_vrai rep2 then 
      begin
      Effet_regle_sur_sequent.impL3_prem c;
      let rep3 = prouvable () in
      Effet_regle_sur_sequent.impL3_rev c;
      if est_vrai rep3 then 
	(true, vrai Preuve.R_impL (snd c) [rep1;rep2;rep3])
      else
	(false, rep3)
      end
    else (true, rep2)
    end
  else (true, rep1)
and impR c =
  Effet_regle_sur_sequent.impR1_prem c;
  let rep1 = prouvable () in
  Effet_regle_sur_sequent.impR1_rev c;
  if est_vrai rep1 then 
    begin
    Effet_regle_sur_sequent.impR2_prem c;
    let rep2 = prouvable () in
    Effet_regle_sur_sequent.impR2_rev c;
    if est_vrai rep2 then 
      (true, vrai Preuve.R_impR (snd c) [rep1;rep2])
    else
      (false, rep2)
    end
  else (true, rep1)

and imp () =
  let nb_g = Seq.nombre_imp_g ()
  and nb_d = Seq.nombre_imp_d () in
  let rec aux_g k acc =
    if k = nb_g then 
      aux_d 0 acc
    else
      let c = Seq.nth_imp_g k in
      let fini, rep = impL c in
      if fini then
	(Seq.reord_imp_g (k+1) nb_g;
	 rep)
      else aux_g (k+1) (rep::acc)
  and aux_d k acc =
    if k = nb_d then 
      cmod acc
    else
      let c = Seq.nth_imp_d k in 
      let fini, rep = impR c in
      if fini then
	(Seq.reord_imp_d (k+1) nb_d;
	 rep)
      else aux_d (k+1) (rep::acc)
  in
  aux_g 0 []
  




(*
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
  

*)
