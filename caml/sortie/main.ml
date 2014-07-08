

let formule = ref None
let attendu = ref None
let ma_liste = ref false
let ma_liste_courte = ref false
let arg1 = ref 0



let details () = Options.preuves := true; Options.cmods := true
let compare_seul () = Options.compare := true; Options.compare_seul := true

let tiroirs_spec = Arg.Tuple [ Arg.Set_int arg1;
  Arg.Int (fun arg2 -> formule := Some (Tiroirs.main !arg1 arg2); attendu := Some (Tiroirs.attendu !arg1 arg2)) ]

let eq_boucle n = formule := Some (Eq_boucle.main n); attendu := Some (Eq_boucle.attendu n)

let options = [
  (* général *)
  (*"-CL", Arg.Set Options.classique, "décide aussi en logique classique";*)
  "-indexation", Arg.Set Options.indexation, "affiche le contenu des tableaux sf et classe";
  "-details", Arg.Unit details, "affiche les preuves et contre-modèles";
  "-preuves", Arg.Set Options.preuves, "affiche les preuves";
  "-cmods", Arg.Set Options.cmods, "affiche les contre-modèles";
  (*"-compare", Arg.Set Options.compare, "compare avec les réponses attendues"; par défaut maintenant*)
  "-compare-seul", Arg.Unit compare_seul, "compare sans afficher les formules et réponses si c'est bon";
  "-rien-afficher", Arg.Set Options.rien_afficher, "n'affiche rien sauf si on trouve un résultat faux";
  "-notime", Arg.Set Options.notime, "ne chronomètre pas";
  "-stop", Arg.Set_float Options.temps_max, "arrête si pas fini pendant une durée donnée";

  (* nom de fichier en .p ou nom de répertoire en contenant *)

  (* autre(s) formule(s) à traiter *)
  "-ph", tiroirs_spec, "principe des tiroirs avec les arguments donnés";
  "-eqb", Arg.Int eq_boucle, "eq_boucle avec les arguments donnés";
  "-liste", Arg.Set ma_liste, "une liste de quelques formules courtes";
  "-liste-courte", Arg.Set ma_liste_courte, "une liste de quelques formules très courtes";

]





let pointp nom = let n = String.length nom in nom.[n-2]='.' && nom.[n-1]='p'

let rec faire_d dnom =
  let dh = Unix.opendir dnom in
  try
    while true do
      let s = Unix.readdir dh in
      if s.[0] <> '.' then faire (dnom^"/"^s)
    done;
  with End_of_file -> ()

and faire nom =
    match (Unix.stat nom).Unix.st_kind with
      | Unix.S_REG when (pointp nom) && (Time.faire_fichier nom) -> Analyseur.main nom
      | Unix.S_DIR -> faire_d nom
      | _ -> ()



let noms = ref []
let () = Arg.parse options (fun nom -> noms := nom:: !noms) ""

let () = List.iter faire (List.rev !noms)


let () = match !formule with Some f -> Exec_formule.main !attendu f | None -> ()

let () =
  if !ma_liste then
    List.iter2 Exec_formule.main Quelques_formules.l_att Quelques_formules.l

let () =
  if !ma_liste_courte then
    List.iter2 Exec_formule.main Quelques_formules.l1_att Quelques_formules.l1

  
let () = if Options.print_temps_total() then Format.printf "temps total : %fs@." !Time.temps_total

(*

let () =
  if !ma_liste then
    if !Options.compare then 
      List.iter2 LSJn.test Quelques_formules.l_att Quelques_formules.l
    else
      List.iter (Init_sf_classe.test ) Quelques_formules.l

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
