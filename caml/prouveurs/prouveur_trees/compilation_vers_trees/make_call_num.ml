open Ast_trees
open Make_fonctions_sf

(*

(* linéaire *)
let appel_sf arg =
  match arg with <arg1,x> =>
  match arg1 with <voulu,actuel> =>
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
      


n connu

let appel_fk arg =
  match arg with <k,x> =>
  
  let m = milieu 0 n in
  if k<m then
    let m = 



n=6
let appel_fk arg =
  match arg with <k,x> =>
  if k<3 then
    if k<1 then
      call f0 x
    else
      if k<2 then
	call f1 x
      else
	call f2 x
  else
    if k<4 then
      call f3 x
    else
      if k<5 then
	call f4 x
      else
	call f5 x
  
*)




let nom k = "f"^(string_of_int k)


let milieu a b = a+(b-a)/2

let rec ifs nom a b =
  if a+1=b then
    ecall (nom a) (evar"x")
  else
    let m = milieu a b in
    eif (eless (evar"k") (eint m))
      (ifs nom a m)
      (ifs nom m b)

let main nom n =
  let body = ematch (evar"arg") "k" "x" (ifs nom 0 n) in
  (nom(-1), "arg", body)

let test n = main nom n

let fonctions_call () =
  let mplus1 = Array.length !Global_ref.sf in
  let rec aux k acc =
    if k=6 then acc else 
      let decl_func = main (Make_fonctions_sf.nom k) mplus1 in
      aux (k+1) (decl_func::acc)
  in aux 0 []


(*call_prem1*)


















