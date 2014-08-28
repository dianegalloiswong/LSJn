open Def
open Global_ref


let rec nombre_noeuds = function
  | FFaux | FVar _ -> 1
  | F (_,f1,f2) -> 1 + (nombre_noeuds f1) + (nombre_noeuds f2)

let main formule =
  let m = nombre_noeuds formule in
  let sf = Array.make (m+1) CFaux in
  let classe = Array.make (m+1) 0 in
  let cl_desc = Array.make (m+1) CFaux in
  let ajout_cd c prochain_cd = 
    let rec cherche i = 
      if i = prochain_cd then -1
      else if cl_desc.(i) = c then i
      else cherche (i+1)
    in
    let i = cherche 0 in
    if i = -1 then
      (cl_desc.(prochain_cd) <- c;
       prochain_cd,prochain_cd+1)
    else
      i,prochain_cd
  in
  let rec remplir prochain prochain_cd = function
    | FFaux -> 
      sf.(prochain) <- CFaux;
      classe.(prochain) <- 0;
      prochain,prochain_cd
    | FVar x ->
      let c = CVar x in
      let cl,pcd = ajout_cd c prochain_cd in
      sf.(prochain) <- c;
      classe.(prochain) <- cl;
      prochain,pcd
    | F (conn,f1,f2) ->
      let i1,pcd1 = remplir prochain prochain_cd f1 in
      let i2,pcd2 = remplir (i1+1) pcd1 f2 in
      let c_cl = C (conn,classe.(i1),classe.(i2)) in
      let cl,pcd = ajout_cd c_cl pcd2 in
      let i = i2+1 in
      sf.(i) <- C (conn,i1,i2);
      classe.(i) <- cl;
      i,pcd
  in
  let i,len_cd = remplir 1 1 formule in
  assert (i = m);
  Global_ref.sf := sf;
  Global_ref.classe := classe


(*
let aligne n = if n<10 then " " else ""

let test f =
  main f;
  (*Format.printf "%s@." (To_string.formule f);*)
  Format.printf "  sf   classe   description@.";
  for i=1 to (Array.length !sf)-1 do
    Format.printf "  %s%d     %s%d      %s@." (aligne i) i (aligne !classe.(i)) !classe.(i) (To_string.case_sf !sf.(i))
  done
*)

