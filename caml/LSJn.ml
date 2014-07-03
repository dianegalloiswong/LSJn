open Def
open Global_ref

open Preuve
open Contre_modele
open Rep


exception Continuer of t
(* quand on trouve une réfutation d'un prémice non inversible (impL et impR) *)



let rec prouvable () = (*Thread.yield ();*)
  Time.verif ();
  match Seq.check_id () with Some a -> Preuve(P (R_Id,a,[])) | None ->
  let qf,c = Seq.choix_formule () in
  match qf with
    | QF_fauxL -> Preuve(P (R_fauxL,snd c,[]))
    | QF_etL -> etL c
    | QF_ouR -> ouR c
    | QF_ouL -> ouL c
    | QF_etR -> etR c
    | QF_imp -> imp ()
    | QF_aucun -> CMod(M (Seq.var_g(),[]))

and inversible1prem r_prem r_rev regle c =
  r_prem c;
  let rep = prouvable () in
  r_rev c;
  if vrai rep then Preuve(P (regle,snd c,preuves[rep])) else rep
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
      Preuve(P (regle,snd c,preuves[rep1;rep2]))
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
	Preuve(P (R_impL,snd c,preuves[rep1;rep2;rep3]))
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
      Preuve(P (R_impR,snd c,preuves[rep1;rep2]))
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
      try impL c with Continuer rep -> aux_g (k+1) (rep::acc)
  and aux_d k acc =
    if k = nb_d then 
      CMod(M (Seq.var_g(),cmods acc))
    else
      let c = Seq.nth_imp_d k in 
      try impR c with Continuer rep -> aux_d (k+1) (rep::acc)
  in
  aux_g 0 []
  



let main formule =
  Init_sf_classe.main formule;
  Init_priorite.main ();

  let m = (Array.length !sf) - 1 in
  Seq.of_sous_formule m;
  (*(match logique with IL -> Seq.of_sous_formule m | CL -> Seq.of_sous_formule_CL m);*)

  prouvable ()






let test attopt f =
  if Options.print_formule() then Format.printf "%s@." (To_string.formule f);
  try
  let rep = Time.time main f in
  match attopt with
    | None -> Rep.print rep
    | Some (batt,_) ->
      let b = vrai rep in
      if b <> batt then
	Format.printf "!!!!!!!!!!!!!!!!!    obtenu : %b    attendu : %b@." b batt
  with Time.Temps_ecoule -> Format.printf "temps écoulé (%fs)@." !Options.temps_max

(*
let test f =
  Format.printf "%s@." (To_string.formule f);
  Format.printf "IL: ";
  let rep = Options.time (main IL) f in
  print_rep rep;
  if !Options.classique then
    (Format.printf "CL: ";
     print_rep (main CL f))


let test_attendu (f,b1,b2) =
  if !Options.classique then
  begin
  let repIL = main IL f and repCL = main CL f in
  let resIL = vrai repIL and resCL = vrai repCL in
  if resIL=b1 && resCL=b2 then
    if !Options.compare_seul then (*Format.printf "OK.@."*)()
    else
      (Format.printf "%s@." (To_string.formule f);
       Format.printf "IL: ";
       print_rep repIL;
       Format.printf "CL: ";
       print_rep repCL;
       Format.printf "OK.@.")
  else 
    Format.printf "%s@.Résultats obtenus :    IL : %b   CL : %b@.Résultats attendus :   IL : %b   CL : %b@." (To_string.formule f) resIL resCL b1 b2
  end

  else
  begin
  let repIL = Options.time (main IL) f in
  let resIL = vrai repIL in
  if resIL=b1 then
    if !Options.compare_seul then (*Format.printf "OK.@."*)()
    else
      (Format.printf "%s@." (To_string.formule f);
       Format.printf "IL: ";
       print_rep repIL;
       Format.printf "OK.@.")
  else 
    Format.printf "%s@.Résultats obtenus :    IL : %b@.Résultats attendus :   IL : %b   CL : %b@." (To_string.formule f) resIL b1 b2
  end

    *)




(*  Format.printf "  IL: %s@." (if vrai(main IL f) then "vrai" else "faux");
  Format.printf "  CL: %s@." (if vrai(main CL f) then "vrai" else "faux")*)
(*
  let pi = deriv_of_formule f in
  (match pi.r with
    | LSJ _ -> 
      Format.printf "vrai@."
      (*Format.printf "vrai@.Preuve :@."; print_deriv_LSJ pi*)
    | RJ _ -> Format.printf "faux@.Contre-modèle :@."; print_arbre (modele_of_RJ pi)
  )
  ;
  Format.printf "CL : %s@." (if cl_valide f then "vrai" else "faux")
*)









(*


  let seq = { g=[]; n=0; d=[(0,f)] } in

  let etL (i,h) = assert (i<=seq.n); match sf.(h) with C(Et,a,b) ->
    let g = Utilities.enleve (i,h) seq.g in
    let g = (i,a)::(i,b)::g in
    seq.g <- g
    | _ -> assert false
  in
  let etL_rev (i,h) = assert (i<=seq.n); match sf.(h) with C(Et,a,b) ->
    let g = Utilities.enleve (i,a) seq.g in
    let g = Utilities.enleve (i,b) g in
    let g = (i,h)::g in
    seq.g <- g
    | _ -> assert false
  in
  let ouR (n,h) = assert (n=seq.n); match sf.(h) with C(Ou,a,b) ->
    let d = Utilities.enleve (n,h) seq.d in
    let d = (n,a)::(n,b)::d in
    seq.d <- d
    | _ -> assert false
  in
  let ouR_rev (n,h) = assert (n=seq.n); match sf.(h) with C(Ou,a,b) ->
    let d = Utilities.enleve (n,a) seq.d in
    let d = Utilities.enleve (n,b) d in
    let d = (n,h)::d in
    seq.d <- d
    | _ -> assert false
  in
  let ouL1 (i,h) = assert (i<=seq.n); match sf.(h) with C(Et,a,_) ->
    let g = Utilities.enleve (i,h) seq.g in
    let g = (i,a)::g in
    seq.g <- g
    | _ -> assert false
  in
  let ouL1_rev (i,h) = assert (i<=seq.n); match sf.(h) with C(Et,a,_) ->
    let g = Utilities.enleve (i,a) seq.g in
    let g = (i,h)::g in
    seq.g <- g
    | _ -> assert false
  in
  let ouL2 (i,h) = assert (i<=seq.n); match sf.(h) with C(Ou,_,b) ->
    let g = Utilities.enleve (i,h) seq.g in
    let g = (i,b)::g in
    seq.g <- g
    | _ -> assert false
  in
  let ouL2_rev (i,h) = assert (i<=seq.n); match sf.(h) with C(Ou,_,b) ->
    let g = Utilities.enleve (i,b) seq.g in
    let g = (i,h)::g in
    seq.g <- g
    | _ -> assert false
  in
  let etR1 (n,h) = assert (n=seq.n); match sf.(h) with C(Et,a,_) ->
    let d = Utilities.enleve (n,h) seq.d in
    let d = (n,a)::d in
    seq.d <- d
    | _ -> assert false
  in
  let etR1_rev (n,h) = assert (n=seq.n); match sf.(h) with C(Et,a,_) ->
    let d = Utilities.enleve (n,a) seq.d in
    let d = (n,h)::d in
    seq.d <- d
    | _ -> assert false
  in
  let etR2 (n,h) = assert (n=seq.n); match sf.(h) with C(Et,_,b) ->
    let d = Utilities.enleve (n,h) seq.d in
    let d = (n,b)::d in
    seq.d <- d
    | _ -> assert false
  in
  let etR2_rev (n,h) = assert (n=seq.n); match sf.(h) with C(Et,_,b) ->
    let d = Utilities.enleve (n,b) seq.d in
    let d = (n,h)::d in
    seq.d <- d
    | _ -> assert false
  in

  let impL1 (i,h) = assert (i<=seq.n); match sf.(h) with C(Imp,_,b) ->
    let g = Utilities.enleve (i,h) seq.g in
    let g = (i,b)::g in
    seq.g <- g
    | _ -> assert false
  in
  let impL1_rev (i,h) = assert (i<=seq.n); match sf.(h) with C(Imp,_,b) ->
    let g = Utilities.enleve (i,b) seq.g in
    let g = (i,h)::g in
    seq.g <- g
    | _ -> assert false
  in
  let impL2 (i,h) = assert (i<=seq.n); match sf.(h) with C(Imp,a,b) ->
    let n = seq.n in
    let g = Utilities.enleve (i,h) seq.g in
    let g = (n+1,b)::g in
    let d = (n,a)::seq.d in
    seq.g <- g; seq.d <- d
    | _ -> assert false
  in
  let impL2_rev (i,h) = assert (i<=seq.n); match sf.(h) with C(Imp,a,b) ->
    let n = seq.n in
    let g = Utilities.enleve (n+1,b) seq.g in
    let g = (i,h)::g in
    let d = Utilities.enleve (n,a) seq.d in
    seq.g <- g; seq.d <- d
    | _ -> assert false
  in
  let impL3 (i,h) = assert (i<=seq.n); match sf.(h) with C(Imp,a,b) ->
    let n = seq.n in
    let g = Utilities.enleve (i,h) seq.g in
    let g = (n+2,b)::g in
    let d = (n+1,a)::seq.d in
    seq.g <- g; seq.d <- d; seq.n <- n+1
    | _ -> assert false
  in
  let impL3_rev (i,h) = assert (i<=seq.n); match sf.(h) with C(Imp,a,b) ->
    let n = seq.n-1 in
    let g = Utilities.enleve (n+2,b) seq.g in
    let g = (i,h)::g in
    let d = Utilities.enleve (n+1,a) seq.d in
    seq.g <- g; seq.d <- d; seq.n <- n
    | _ -> assert false
  in


  let check_id () =
    List.exists (fun (i,a) -> i<=seq.n &&
      List.exists (fun (i,b) -> i=seq.n && classe.(a)=classe.(b)) seq.d
    ) seq.g
  in

*)
