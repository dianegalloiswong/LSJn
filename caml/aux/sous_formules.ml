open Def


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
  Def.sf := sf;
  Def.classe := classe


let test f =
  main f;
  Format.printf "@.%s :@." (Utilities.formule_to_string f);
  Utilities.print_sous_formules !sf !classe





(*

let tab_of_formule formule =
  let t = Array.make (nombre_noeuds formule) CFaux in
  let ajout_case c libre =
    let rec cherche i = 
      if i = libre then -1
      else if t.(i) = c then i
      else cherche (i+1)
    in
    let i = cherche 0 in
    if i = -1 then
      (t.(libre) <- c;
       libre,libre+1)
    else
      i,libre
  in
  let rec remplir f libre = match f with
    | FFaux -> ajout_case CFaux libre
    | FVar x -> ajout_case (CVar x) libre
    | F(conn,f1,f2) ->
      let libre1,i1 = remplir f1 libre in
      let libre2,i2 = remplir f2 libre in
      ajout_case (C (conn,i1,i2)) libre2
  in
  let i,len = remplir formule 0 in
  let t' = Array.make len CFaux in
  for k=0 to len-1 do t'.(k)<-t.(k) done;
  t',i

*)
