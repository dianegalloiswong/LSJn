(*
utilities.ml

equal : <x,y> -> booléen x=y égalité structurelle
compare : <x,y> -> booléen x<=y. null < tous les entiers par ordre croissants < tous les couples par ordre lexicographique

pred : n -> n-1

length : l -> longueur de la liste l

rm : <x,l> -> liste l privée de l'élément x
mem : <x,l> -> booléen : x appartient à liste l
mem_inf <<n,x>,l> -> booléen : l contient un couple <i,x> avec i<=n (i,n entiers)

sort : l -> l triée (insertion)

filter : l -> l' liste des éléments de l qui vérifient condition (fonction inexistante, c'est juste un exemple)

nth : <n,l> -> le n-ième élément de l (commence à 0)
*)
let equal arg =
  match arg with <x,y> =>
  if isnull x && isnull y then 1 else
  if isint x && isint y && x=y then 1 else
  if isnode x && isnode y then
    match x with <x1,x2> => match y with <y1,y2> =>
    (call equal <x1,y1>) && (call equal <x2,y2>)
  else 0

let compare arg =
  match arg with <x,y> =>
  if isnull x then 1 else if isnull y then 0 else
  if isint x && isint y then x<=y else
  if isint x then 1 else if isint y then 0 else
  match x with <x1,x2> => match y with <y1,y2> =>
  if call equal <x1,y1> then call compare <x2,y2>
  else call compare <x1,y1>

let pred_aux arg =
  match arg with <n,arg2> =>
  match arg2 with <i,j> =>
  if j=n then i
  else call pred_aux <n,<i+1,j+1>>
let pred n =
  if n<=0 then 0 else
  call pred_aux <n,<0,1>>

(***)

let length l =
  if isnull l then 0 else
  match l with <h,t> =>
  call length t +1

let rm arg =
  match arg with <x,l> =>
  match l with <h,t> =>
  if call equal <h,x> then t else <h,call rm <x,t>>

let mem arg =
  match arg with <x,l> =>
  if isnull(l) then 0 else
    match l with <hd,tl> =>
    if call equal <hd,x> then 1 else call mem <x,tl>

let mem_inf arg =
  match arg with <arg1,l> =>
  if isnull(l) then 0 else
    match arg1 with <n,x> =>
    match l with <hd,tl> =>
    match hd with <i,y> =>
    if (i<=n && call equal <y,x>) then 1 else call mem_inf <arg1,tl>

let insert arg =
  match arg with <x,l> =>
  if isnull l then <x,null> else
  match l with <h,t> =>
  if call compare <x,h> then
    <x,l>
  else
    <h, call insert <x,t> >

let sort l =
  if isnull l then l else
  match l with <h,t> =>
  let t = call sort t in
  call insert <h,t>

(*
let filter l =
  if isnull l then l else
  match l with <h,t> =>
  let t = call filter t in
  if call condition h then <h,t> else t
*)

let nth arg =
  match arg with <n,l> =>
  if isnull l then null else
  match l with <h,t> =>
  if n<=0 then h else call nth <call pred n,t>
(*
operations_sequent.ml

   sequent : <formules,reste>
   formules : <G,D>
   G/D : liste de <indice,a> où a : <priorite,sf>
   reste : <classes,infos>
   classes : <CG,CD>
   CG/CD : liste de <indice,cl>
   infos : <n,axiomes>
   n : indice du sequent
   axiomes : <fauxL,id>

n_of_seq : seq -> n
G_of_seq : seq -> G
D_of_seq : seq -> D

set_n : <n,seq> -> seq où l'indice vaut n; ne s'occupe pas des axiomes

rm_ax : seq -> seq où les axiomes sont mis à faux
set_fauxL : seq -> seq où fauxL=vrai
set_id : seq -> seq où id=vrai

there_is_ax : seq -> booléen au moins un axiome est vrai

incr_n (s'occupe de fauxL)
decr_n (ne s'occupe pas des axiomes)

add_cl_g <c,seq> -> seq : ajoute c=(i,cl) à CG, et si axiomes, met à jour
add_cl_d
rm_cl_g
rm_cl_d

add_g < <<i,a>,cl> , seq > ajoute (i,a) à G et appelle add_cl_g avec (i,cl)
  ( ajoute (i,cl) à CG, et fauxL:=vrai si cl=0 et i<=n, et id:=vrai si i<=n et (n,cl) présent dans CD )
add_d
rm_g
rm_d

empty_seq : _ -> sequent vide
*)


let n_of_seq seq =
  match seq with <formules,reste> =>
  match reste with <classes,infos> =>
  match infos with <n,axiomes> =>
  n
let G_of_seq seq =
  match seq with <formules,reste> =>
  match formules with <G,D> =>
  G
let D_of_seq seq =
  match seq with <formules,reste> =>
  match formules with <G,D> =>
  D

let set_n arg =
  match arg with <n,seq> =>
  match seq with <formules,reste> =>
  match reste with <classes,infos> =>
  match infos with <n1,axiomes> =>
  <formules, <classes, <n, axiomes > > >

let rm_ax seq =
  match seq with <formules,reste> =>
  match reste with <classes,infos> =>
  match infos with <n,axiomes> =>
  <formules, <classes, <n, <0,0> > > >

let set_fauxL seq =
  match seq with <formules,reste> =>
  match reste with <classes,infos> =>
  match infos with <n,axiomes> =>
  match axiomes with <fauxL,id> =>
  <formules, <classes, <n, <1,id> > > >

let set_id seq =
  match seq with <formules,reste> =>
  match reste with <classes,infos> =>
  match infos with <n,axiomes> =>
  match axiomes with <fauxL,id> =>
  <formules, <classes, <n, <fauxL,1> > > >

let there_is_ax seq =
  match seq with <formules,reste> =>
  match reste with <classes,infos> =>
  match infos with <n,axiomes> =>
  match axiomes with <fauxL,id> =>
  fauxL || id

(***)

let incr_n seq =
  match seq with <formules,reste> =>
  match reste with <classes,infos> =>
  match infos with <n,axiomes> =>
  let seq = <formules, <classes, <n+1, axiomes > > > in
  match classes with <CG,CD> =>
  if call mem <<n+1,0>,CG> then call set_fauxL seq
  else seq

let decr_n seq =
  match seq with <formules,reste> =>
  match reste with <classes,infos> =>
  match infos with <n,axiomes> =>
  let nmoins1 = call pred n in
  let seq = <formules, <classes, <nmoins1, axiomes > > > in
  seq

(***)

let add_cl_g arg =
  match arg with <c,seq> =>
  match seq with <formules,reste> =>
  match reste with <classes,infos> =>
  match classes with <CG,CD> =>
  let CG = <c,CG> in
  let seq = <formules, < <CG,CD>, infos> > in

  match c with <i,cl> =>
  match infos with <n,axiomes> =>
  if i<=n then
    if cl=0 then call set_fauxL seq 
    else if call mem <<n,cl>,CD> then call set_id seq 
    else seq
  else seq

let add_cl_d arg =
  match arg with <c,seq> =>
  match seq with <formules,reste> =>
  match reste with <classes,infos> =>
  match classes with <CG,CD> =>
  let CD = <c,CD> in
  let seq = <formules, < <CG,CD>, infos> > in

  match c with <i,cl> =>
  match infos with <n,axiomes> =>
  if i=n && call mem_inf <<n,cl>,CG> then call set_id seq 
  else seq

let rm_cl_g arg =
  match arg with <c,seq> =>
  match seq with <formules,reste> =>
  match reste with <classes,infos> =>
  match classes with <CG,CD> =>
  let CG = call rm <c,CG> in
  let seq = <formules, < <CG,CD>, infos> > in
  seq

let rm_cl_d arg =
  match arg with <c,seq> =>
  match seq with <formules,reste> =>
  match reste with <classes,infos> =>
  match classes with <CG,CD> =>
  let CD = call rm <c,CD> in
  let seq = <formules, < <CG,CD>, infos> > in
  seq

(***)

let add_g arg =
  match arg with <arg1,seq> =>
  match arg1 with <c,cl> =>
  match c with <i,a> =>
  match seq with <formules,reste> =>
  match formules with <G,D> =>
  let G = <c,G> in
  let seq = <<G,D>,reste> in
  call add_cl_g <<i,cl>,seq>

let add_d arg =
  match arg with <arg1,seq> =>
  match arg1 with <c,cl> =>
  match c with <i,a> =>
  match seq with <formules,reste> =>
  match formules with <G,D> =>
  let D = <c,D> in
  let seq = <<G,D>,reste> in
  call add_cl_d <<i,cl>,seq>

let rm_g arg =
  match arg with <arg1,seq> =>
  match arg1 with <c,cl> =>
  match c with <i,a> =>
  match seq with <formules,reste> =>
  match formules with <G,D> =>
  let G = call rm <c,G> in
  let seq = <<G,D>,reste> in
  call rm_cl_g <<i,cl>,seq>

let rm_d arg =
  match arg with <arg1,seq> =>
  match arg1 with <c,cl> =>
  match c with <i,a> =>
  match seq with <formules,reste> =>
  match formules with <G,D> =>
  let D = call rm <c,D> in
  let seq = <<G,D>,reste> in
  call rm_cl_d <<i,cl>,seq>

(***)

let empty_seq arg =
  <  <null,null>  ,  < <null,null> , <0,<0,0>> >  >

(*
fichier fonctions_sf

non (faux)
  sf   classe   description
   1      0      Faux
   2      0      Faux
   3      1      1 -> 2

*)
let sf3_prem1 arg = 
(match arg with <i,seq> => 
(let seq = (call rm_d < < < i , < 5 , 3 > > , 1 > , seq >) in 
(let seq = (call add_d < < < i , < 6 , 2 > > , 0 > , seq >) in 
(let seq = (call add_g < < < i , < 0 , 1 > > , 0 > , seq >) in 
seq))))

let sf3_rev1 arg = 
(match arg with <i,seq> => 
(let seq = (call rm_g < < < i , < 0 , 1 > > , 0 > , seq >) in 
(let seq = (call rm_d < < < i , < 6 , 2 > > , 0 > , seq >) in 
(let seq = (call add_d < < < i , < 5 , 3 > > , 1 > , seq >) in 
(let seq = (call rm_ax seq) in 
seq)))))

let sf3_prem2 arg = 
(match arg with <i,seq> => 
(let seq = (call incr_n seq) in 
(let i = (i +1) in 
(let seq = (call rm_d < < < i , < 5 , 3 > > , 1 > , seq >) in 
(let seq = (call add_d < < < i , < 6 , 2 > > , 0 > , seq >) in 
(let seq = (call add_g < < < i , < 0 , 1 > > , 0 > , seq >) in 
seq))))))

let sf3_rev2 arg = 
(match arg with <i,seq> => 
(let i = (i +1) in 
(let seq = (call rm_g < < < i , < 0 , 1 > > , 0 > , seq >) in 
(let seq = (call rm_d < < < i , < 6 , 2 > > , 0 > , seq >) in 
(let seq = (call add_d < < < i , < 5 , 3 > > , 1 > , seq >) in 
(let seq = (call rm_ax seq) in 
(let seq = (call decr_n seq) in 
seq)))))))

let ajout_formule_initiale seq = 
(call add_d < < < 0 , < 5 , 3 > > , 1 > , seq >)

let call_rev3 arg = 
(match arg with <k,x> => 
(if (k<2) then 
(if (k<1) then 
(call sf0_rev3 x) else 
(call sf1_rev3 x)) else 
(if (k<3) then 
(call sf2_rev3 x) else 
(call sf3_rev3 x))))

let call_prem3 arg = 
(match arg with <k,x> => 
(if (k<2) then 
(if (k<1) then 
(call sf0_prem3 x) else 
(call sf1_prem3 x)) else 
(if (k<3) then 
(call sf2_prem3 x) else 
(call sf3_prem3 x))))

let call_rev2 arg = 
(match arg with <k,x> => 
(if (k<2) then 
(if (k<1) then 
(call sf0_rev2 x) else 
(call sf1_rev2 x)) else 
(if (k<3) then 
(call sf2_rev2 x) else 
(call sf3_rev2 x))))

let call_prem2 arg = 
(match arg with <k,x> => 
(if (k<2) then 
(if (k<1) then 
(call sf0_prem2 x) else 
(call sf1_prem2 x)) else 
(if (k<3) then 
(call sf2_prem2 x) else 
(call sf3_prem2 x))))

let call_rev1 arg = 
(match arg with <k,x> => 
(if (k<2) then 
(if (k<1) then 
(call sf0_rev1 x) else 
(call sf1_rev1 x)) else 
(if (k<3) then 
(call sf2_rev1 x) else 
(call sf3_rev1 x))))

let call_prem1 arg = 
(match arg with <k,x> => 
(if (k<2) then 
(if (k<1) then 
(call sf0_prem1 x) else 
(call sf1_prem1 x)) else 
(if (k<3) then 
(call sf2_prem1 x) else 
(call sf3_prem1 x))))

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
(*
fichier all_imp

x : <i,<p,sf>> (les éléments de G et D)

is_imp_g : <n,x> -> i<=n et p=5
is_imp_d

filter_is_imp_g : <n,l> -> les éléments de l avec i<=n et p=5
filter_is_imp_d : <n,l> -> les éléments de l avec i=n et p=5

nb_imp_g
nb_imp_d

nth_imp_g : renvoie les éléments <i,sf> du n-ième implique de G
nth_imp_d

*)

let is_imp_g arg =
  match arg with <n,x> =>
  match x with <i,a> =>
  i<=n && (match a with <p,sf> => p=5)

let is_imp_d arg =
  match arg with <n,x> =>
  match x with <i,a> =>
  i=n && (match a with <p,sf> => p=5)

let filter_is_imp_g arg =
  match arg with <n,l> =>
  if isnull l then l else
  match l with <h,t> =>
  let t = call filter_is_imp_g <n,t> in
  if call is_imp_g <n,h> then <h,t> else t

let filter_is_imp_d arg =
  match arg with <n,l> =>
  if isnull l then l else
  match l with <h,t> =>
  let t = call filter_is_imp_d <n,t> in
  if call is_imp_d <n,h> then <h,t> else t

let all_imp_g seq =
  let n = call n_of_seq seq in
  let G = call G_of_seq seq in
  call filter_is_imp_g <n,G>

let all_imp_d seq =
  let n = call n_of_seq seq in
  let D = call D_of_seq seq in
  call filter_is_imp_d <n,D>

let nb_imp_g seq =
  call length (call all_imp_g seq)
let nb_imp_d seq =
  call length (call all_imp_d seq)

let nth_imp_g arg =
  match arg with <k,seq> =>
  let x = call nth <k, call all_imp_g seq> in
  match x with <i,a> =>
  match a with <p,sf> =>
  <i,sf>
let nth_imp_d arg =
  match arg with <k,seq> =>
  let x = call nth <k, call all_imp_d seq> in
  match x with <i,a> =>
  match a with <p,sf> =>
  <i,sf>
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
(* fichier expr *)
in
(*let l = <4,<7,<2,<8,<4,<<3,2>,<<3,3>,<1,<null,null>>>>>>>>> in*)
(*call sort l*)
(*call nth <5,l>*)
(*call length l*)

let seq = call empty_seq null in
let seq = call ajout_formule_initiale seq in
call prouvable seq
