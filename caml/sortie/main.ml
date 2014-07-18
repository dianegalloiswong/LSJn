

let formule = ref None
let attendu = ref None
let ma_liste = ref false
let ma_liste_courte = ref false
let arg1 = ref 0

(*let details () = Options.preuves := true; Options.cmods := true*)

let tiroirs_spec = Arg.Tuple [ Arg.Set_int arg1;
  Arg.Int (fun arg2 -> formule := Some (Tiroirs.main !arg1 arg2); attendu := Some (Tiroirs.attendu !arg1 arg2)) ]

let eq_boucle n = formule := Some (Eq_boucle.main n); attendu := Some (Eq_boucle.attendu n)

let options = [
  "-trees", Arg.Set Options.trees, ": utilise le prouveur \"trees\" au lieu du prouveur caml";

  (* général *)
  "-indexation", Arg.Set Options.indexation, ": affiche formules et contenu des tableaux sf et classe ; ne lance pas la recherche de preuve";
  "-f", Arg.Set Options.affiche_formule_ref, ": affiche la formule";
  "-rep", Arg.Set Options.affiche_rep_ref, ": affiche le résultat (\"vrai\" ou \"faux\")";
  "-rien-afficher", Arg.Set Options.rien_afficher, ": n'affiche que le temps total, sauf si on trouve un résultat faux";
  "-notime", Arg.Set Options.notime, ": ne chronomètre pas";
  "-stop", Arg.Set_float Options.temps_max, "<f:float> : arrête si pas fini après f secondes";
  (*
  "-details", Arg.Unit details, "affiche les preuves et contre-modèles";
  "-preuves", Arg.Set Options.preuves, "affiche les preuves";
  "-cmods", Arg.Set Options.cmods, "affiche les contre-modèles";
  *)

  (* nom de fichier en .p ou nom de répertoire en contenant *)

  (* autre(s) formule(s) à traiter *)
  "-ph", tiroirs_spec, "<p:int> <h:int> : exécute sur le principe du pigeonnier avec p pigeons et h trous";
  "-eqb", Arg.Int eq_boucle, "<n:int> : exécute sur une boucle de n équivalences";
  "-liste", Arg.Set ma_liste, ": exécute sur une liste de quelques formules courtes";
  "-liste-courte", Arg.Set ma_liste_courte, ": exécute sur une liste de quelques formules très courtes";

  (* debug *)
  "-irr", Arg.Set Options.irr, ": affiche les utilisations de l'axiome de réfutation Irr";

  "-comp", Arg.Set Options.compile_test, ": avec la simulation de recherche compilée";
]

let usage = "Par défaut : affiche nom de fichier éventuel, temps mis pour décider si la formule est prouvable, et résultat obtenu seulement s'il est différent de celui attendu.\n  <nom de fichier en .p> (syntaxe des problèmes d'ILTP) : exécute sur la formule décrite par le fichier, en comparant avec le résultat attendu donné dans les commentaires\n  <nom de répertoire> : cherche récursivement les fichiers en .p"


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
      | Unix.S_REG when (pointp nom) && (Time.faire_fichier nom) -> Exec_ILTP.main nom
      | Unix.S_DIR -> faire_d nom
      | _ -> ()



let noms = ref []
let () = Arg.parse options (fun nom -> noms := nom:: !noms) usage

let () = List.iter faire (List.rev !noms)


let () = match !formule with Some f -> Exec_formule.main !attendu f | None -> ()

let () =
  if !ma_liste then
    List.iter2 Exec_formule.main Quelques_formules.l_att Quelques_formules.l

let () =
  if !ma_liste_courte then
    List.iter2 Exec_formule.main Quelques_formules.l1_att Quelques_formules.l1

  
let () = 
  if Options.affiche_temps_total() then Format.printf "temps total : %fs@." !Time.temps_total;
  if !Time.echoues > 0 then Format.printf "non terminés en moins de %fs : %d@." !Options.temps_max !Time.echoues;
  if !Time.non_traites > 0 then Format.printf "non traités (car problème plus facile non terminé) : %d@." !Time.non_traites




