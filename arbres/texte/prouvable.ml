(*
fichier prouvable

prouvable : seq -> booléen
inv1prem
inv2prem
impL
impR
imp_aux_g
imp_aux_d
*)

let prouvable seq =
  if call there_is_ax seq then 1 else
  let x = call plus_forte_priorite seq in
  match x with <p,i_sf> =>
  if p=1 || p=2 then
    call inv1prem <i_sf,seq>
  else if p=3 || p=4 then
    call inv2prem <i_sf,seq>
  else if p=5 then call imp seq
  else 0

let inv1prem arg =
  match arg with <i_sf,seq> =>
  match i_sf with <i,sf> =>
  let seq = call call_prem1 <sf,<i,seq>> in
  let rep = call prouvable seq in
  let seq = call call_rev1 <sf,<i,seq>> in
  rep

let inv2prem arg =
  match arg with <i_sf,seq> =>
  match i_sf with <i,sf> =>
  let seq = call call_prem1 <sf,<i,seq>> in
  let rep1 = call prouvable seq in
  let seq = call call_rev1 <sf,<i,seq>> in
  if rep1 then
    let seq = call call_prem2 <sf,<i,seq>> in
    let rep2 = call prouvable seq in
    let seq = call call_rev2 <sf,<i,seq>> in
    rep2
  else
    0


let impL arg =
  match arg with <i_sf,seq> =>
  match i_sf with <i,sf> =>
  let seq = call call_prem1 <sf,<i,seq>> in
  let rep1 = call prouvable seq in
  let seq = call call_rev1 <sf,<i,seq>> in
  if rep1 then
    let seq = call call_prem2 <sf,<i,seq>> in
    let rep2 = call prouvable seq in
    let seq = call call_rev2 <sf,<i,seq>> in
    if rep2 then
      let seq = call call_prem3 <sf,<i,seq>> in
      let rep3 = call prouvable seq in
      let seq = call call_rev3 <sf,<i,seq>> in
      if rep3 then
	<1,1>
      else
	<0,0>
    else
      <1,0>
  else
    <1,0>


let impR arg =
  match arg with <i_sf,seq> =>
  match i_sf with <i,sf> =>
  let seq = call call_prem1 <sf,<i,seq>> in
  let rep1 = call prouvable seq in
  let seq = call call_rev1 <sf,<i,seq>> in
  if rep1 then
    let seq = call call_prem2 <sf,<i,seq>> in
    let rep2 = call prouvable seq in
    let seq = call call_rev2 <sf,<i,seq>> in
    if rep2 then
      <1,1>
    else
      <0,0>
  else
    <1,0>

let imp_aux_g arg =
  match arg with <arg1,seq> =>
  match arg1 with <k,nbs> =>
  match nbs with <nb_g,nb_d> =>
  if k=nb_g then
    call imp_aux_d <<0,nb_d>,seq>
  else
    let i_sf = call nth_imp_g <k,seq> in
    match (call impL <i_sf,seq>) with <fini,rep> =>
    if fini then rep else call imp_aux_g <<k+1,nbs>,seq>

let imp_aux_d arg =
  match arg with <arg1,seq> =>
  match arg1 with <k,nb_d> =>
  if k=nb_d then
    0
  else
    let i_sf = call nth_imp_d <k,seq> in
    match (call impR <i_sf,seq>) with <fini,rep> =>
    if fini then rep else call imp_aux_d <<k+1,nb_d>,seq>

let imp seq =
  let nb_g = call nb_imp_g seq in
  let nb_d = call nb_imp_d seq in
  call imp_aux_g <<0,<nb_g,nb_d>>,seq>
