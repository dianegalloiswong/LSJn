open Arbres_ast
open Fonctions_de_sf



let appel_sf arg =
  match arg with <arg1,x> =>
  match arg1 with <voulu,actuel> =>
  if actuel=voulu then
    call nom_de_fonction_correspondant x
  else
    call appel_sf <<voulu,actuel+1>,x>


let appel_sf arg =
  match arg with <arg1,x> =>
  match arg1 with <k,bornes> =>
  match bornes with <inf,sup> =>
  if k=inf then
    call nom_de_fonction_inf x
  else
    let mi = call milieu bornes in
    if mi<=k then 
      ...<mi,sup>
    else
      ...<inf,mi>


  if actuel=voulu then
    call nom_de_fonction_correspondant x
  else
    call appel_sf <<voulu,actuel+1>,x>
      

let milieu arg =
  match arg with <a,b> =>
  if a<=b then
    let a = a+1 in
    let b = b-1 in
    if b<=a then 
      b
    else
      call milieu <a,b>
  else
    call milieu <b,a>






let appel_fk arg =
  
























