(*
equal : <x,y> -> x=y égalité structurelle

rm : <x,l> -> liste l privée de l'élément x
mem : <x,l> -> booléen : x appartient à l
mem_inf <<n,x>,l> -> booléen : l contient un couple <i,x> avec i<=n

n_of_seq : seq -> n

(**)
add_cl_g <c,seq> -> seq : ajoute c=(i,cl) à CG, et si axiomes, met à jour
add_cl_d
rm_cl_g
rm_cl_d

add_g < <<i,a>,cl> , seq > ajoute (i,a) à G et appelle add_cl_g avec (i,cl)
  ( ajoute (i,cl) à CG, et fauxL:=vrai si cl=0 et i<=n, et id:=vrai si i<=n et (n,cl) présent dans CD )

rm_ax : seq -> seq où les axiomes sont mis à faux

incr_n
decr_n
*)


let equal arg =
  match arg with <x,y> =>
  if isnull x && isnull y then 1 else
  if isint x && isint y && x=y then 1 else
  if isnode x && isnode y then
    match x with <x1,x2> => match y with <y1,y2> =>
    (call equal <x1,y1>) && (call equal <x2,y2>)
  else 0


let rm arg =
  match arg with <x,l> =>
  if isnull l then l else (**)
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




let n_of_seq seq =
  match seq with <formules,reste> =>
  match reste with <classes,infos> =>
  match infos with <n,axiomes> =>
  n

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


let add_g arg =
  match arg with <arg1,seq> =>
  match arg1 with <c,cl> =>
  match c with <i,a> =>
  match seq with <formules,reste> =>
  match formules with <G,D> =>
  let G = <c,G> in
  let seq = <<G,D>,reste> in
  call add_cl_g <<i,cl>,seq>







(* -------------------- *)


in

(*call rm <2,  <1,<2,<3,null>>> >*)

let l = null in
let l = < <1,2>, l > in
let l = < <3,2>, l > in
let l = < <2,1>, l > in
let l = < <1,4>, l > in
let l = < <3,2>, l > in
let l = < <4,4>, l > in
let l = < <6,5>, l > in

(*call rm < <1,1>, l >*)
call mem_inf < <2,4>, l >
