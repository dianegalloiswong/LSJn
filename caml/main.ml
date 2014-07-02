

let formule = ref None
let ma_liste = ref false
let ma_liste_courte = ref false
let arg1 = ref 0



let details () = Options.preuves := true; Options.cmods := true
let compare_seul () = Options.compare := true; Options.compare_seul := true

let tiroirs_spec = Arg.Tuple [ Arg.Set_int arg1;
  Arg.Int (fun arg2 -> formule := Some (Tiroirs.main !arg1 arg2)) ]

let eq_boucle n = formule := Some (Eq_boucle.main n)

let options = [
  (* général *)
  "-CL", Arg.Set Options.classique, "décide aussi en logique classique";
  "-details", Arg.Unit details, "affiche les preuves et contre-modèles";
  "-preuves", Arg.Set Options.preuves, "affiche les preuves";
  "-cmods", Arg.Set Options.cmods, "affiche les contre-modèles";
  "-compare", Arg.Set Options.compare, "compare avec les réponses attendues";
  "-compare-seul", Arg.Unit compare_seul, "compare sans afficher les formules et réponses si c'est bon";

  (* formule(s) à traiter *)
  "-ph", tiroirs_spec, "principe des tiroirs avec les arguments donnés";
  "-eqb", Arg.Int eq_boucle, "eq_boucle avec les arguments donnés";
  "-liste", Arg.Set ma_liste, "une liste de quelques formules courtes";
  "-liste-courte", Arg.Set ma_liste_courte, "une liste de quelques formules très courtes";

  (* ou un nom de fichier/répertoire sans flag *)
]



let () = Arg.parse options Analyseur.main ""



let () = match !formule with Some f -> LSJn.test f | None -> ()

let () =
  if !ma_liste then
    if !Options.compare then 
      List.iter (fun f ->LSJn.test_attendu f) Quelques_formules.l_att
    else
      List.iter (fun f ->LSJn.test f) Quelques_formules.l
let () =
  if !ma_liste_courte then
    if !Options.compare then 
      List.iter (fun f ->LSJn.test_attendu f) Quelques_formules.l1_att
    else
      List.iter (fun f ->LSJn.test f) Quelques_formules.l1
  


(*

let f =
  if !arg_eqb > 0 then
    Some (Eq_boucle.main !arg_eqb)
  else if !arg1_tir > 0 then
    Some (Tiroirs.main !arg1_tir !arg2_tir)
  else
    None

let () =
(*  if !fichier <> "" then
    Analyseur.main !fichier
  else*)
  if not !fichier then
    (match f with
      | Some f -> LSJn.test f
      | None ->
	if !Options.compare then 
	  List.iter (fun f ->LSJn.test_attendu f) l_att
	else if !courts then
	  List.iter (fun f ->LSJn.test f) l1
	else
	  List.iter (fun f ->LSJn.test f) l
    );
  Format.printf "@."

*)


(*
  "-tiroirs", tiroirs_spec, "principe des tiroirs avec les arguments donnés";
  "-tir", tiroirs_spec, "principe des tiroirs avec les arguments donnés";
  "-pigeonhole", tiroirs_spec, "principe des tiroirs avec les arguments donnés";
*)
