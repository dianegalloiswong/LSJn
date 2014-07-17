(*
fichier plus_forte_priorite

G,D du séquent : listes de (x : <i,<p,sf>>)
  i : indice, p : priorite, sf : numero de sous-formule

fold_plus_forte_priorite_g : <courant,<n,l>> : renvoie le x de plus petite p verifiant i<=n de l si p plus petite que celle de courant, sinon courant
fold_plus_forte_priorite_d

plus_forte_priorite : seq -> les composantes du x de G ou D de plus forte priorite, mais cette fois agences sous la forme <p,<i,sf>>

*)


let fold_plus_forte_priorite_g arg =
  match arg with <courant,arg2> =>
  match arg2 with <n,l> =>
  if isnull l then courant else
  match l with <h,t> =>
  match h with <i,a> =>
  let courant =
    if not (i<=n) then courant else
    match a with <p,sf> =>
    match courant with <ic,ac> =>
    match ac with <pc,sfc> =>
    if p<pc then h else courant
  in
  call fold_plus_forte_priorite_g <courant,<n,t>>

let fold_plus_forte_priorite_d arg =
  match arg with <courant,arg2> =>
  match arg2 with <n,l> =>
  if isnull l then courant else
  match l with <h,t> =>
  match h with <i,a> =>
  let courant =
    if not (i=n) then courant else
    match a with <p,sf> =>
    match courant with <ic,ac> =>
    match ac with <pc,sfc> =>
    if p<pc then h else courant
  in
  call fold_plus_forte_priorite_d <courant,<n,t>>

let plus_forte_priorite seq =
  let n = call n_of_seq seq in
  let G = call G_of_seq seq in
  let D = call D_of_seq seq in
  let x = call fold_plus_forte_priorite_g <<0,<7,0>>,<n,G>> in
  let x = call fold_plus_forte_priorite_d <x,<n,D>> in
  match x with <i,a> =>
  match a with <p,sf> =>
  <p,<i,sf>>
