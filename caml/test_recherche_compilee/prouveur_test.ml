open Def
open Global_ref
open Precalculs
open Aux





let rec prouvable seq =
  Time.verif_timeout ();
  if seq.fauxL || seq.id then true else
  let p,c = plus_forte_priorite seq in
  match p with 
    | 0 -> assert false
    | 1 -> inv1prem c seq
    | 2 -> inv1prem c seq
    | 3 -> inv2prem c seq
    | 4 -> inv2prem c seq
    | 5 -> imp seq
    | 6 -> false
    | _ -> assert false


and inv1prem (i,h) seq =
  match !fonctions.(h) with [prem;rev] ->
    prem i seq;
    let rep = prouvable seq in
    rev i seq;
    rep
    | _ -> assert false

and inv2prem (i,h) seq =
  match !fonctions.(h) with [prem1;rev1;prem2;rev2] ->
    prem1 i seq;
    let rep1 = prouvable seq in
    rev1 i seq;
    if rep1 then
      begin
      prem2 i seq;
      let rep2 = prouvable seq in
      rev2 i seq;
      rep2
      end
    else
      false
    | _ -> assert false



and impL (i,h) seq =
  match !fonctions.(h) with [prem1;rev1;prem2;rev2;prem3;rev3] ->
    prem1 i seq;
    let rep1 = prouvable seq in
    rev1 i seq;
    if rep1 then
      begin
      prem2 i seq;
      let rep2 = prouvable seq in
      rev2 i seq;
      if rep2 then
	begin
	prem3 i seq;
	let rep3 = prouvable seq in
	rev3 i seq;
	if rep3 then
	  true,true
	else
	  false,false
	end
      else
	true,false
      end
    else
      true,false
    | _ -> assert false

and impR (i,h) seq =
  match !fonctions.(h) with [prem1;rev1;prem2;rev2] ->
    prem1 i seq;
    let rep1 = prouvable seq in
    rev1 i seq;
    if rep1 then
      begin
      prem2 i seq;
      let rep2 = prouvable seq in
      rev2 i seq;
      if rep2 then
	true,true
      else
	false,false
      end
    else
      true,false
    | _ -> assert false

and imp seq =
  let nb_g = nb_imp_g seq
  and nb_d = nb_imp_d seq in
  let rec aux_g k =
    if k = nb_g then 
      aux_d 0
    else
      let c = nth_imp_g k seq in
      let fini, rep = impL c seq in
      if fini then rep
      else aux_g (k+1)
  and aux_d k =
    if k = nb_d then 
      false
    else
      let c = nth_imp_d k seq in 
      let fini, rep = impR c seq in
      if fini then rep
      else aux_d (k+1)
  in
  aux_g 0









let main formule =
  Init_sf_classe.main formule;
  Init_priorite.main ();
  remplir_cote ();
  remplir_fonctions ();
  let m = Array.length !sf - 1 in
  let seq = match !sf.(m) with
    | C (Imp,a,b) -> { g=[(0,a)]; n=0; d=[(0,b)]; fauxL=(!classe.(a)=0); id=(!classe.(a)= !classe.(b)) }
    | _ -> { g=[]; n=0; d=[(0,m)]; fauxL=false; id=false }
  in
  let b = prouvable seq in
  Rep.Bool b

