open Ast_trees

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

let main () =
  let mplus1 = Array.length !Global_ref.sf in
  let rec aux k acc =
    if k=6 then acc else 
      let decl_func = main (Fonctions_compilees.nom k) mplus1 in
      aux (k+1) (decl_func::acc)
  in
  Fonctions_compilees.fonctions := (aux 0 [])@ !Fonctions_compilees.fonctions



(*
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















